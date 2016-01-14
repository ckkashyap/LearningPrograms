#!/usr/bin/perl -w

use strict;
use HTML::Entities;

# input is HTML output from Markdown with the following extras:
#   - * @@pause before this bullet
#   - <!-- comment --> (those spaces are needed.  A comment can span lines,
#     but any non-comment stuff on the same line will get deleted)
#   - <!-- begin metadata
#       title=some long title
#       shorttitle=optional shorter version
#       subtitle=optional subtitle
#       occasion=name of meeting or event
#       shortoccasion=optional short name of event
#       date=optional date string (defaults to yyyy-mm-dd if not given)
#       tictoc=true (if you want every bullet in pause mode)
#       toctoc=true (if you want TOCs before each section/subsection)
#       style=stylename (default may be Darmstadt)
#     end metadata --> (each metadata item extends to end of line)
#   - <!-- begin graphviz fig-foo
#       graphviz code
#     end graphviz -->, which you then use like this (although the width /
#     height spec itself is optional, the brackets are part of its syntax).
#     Then you say
#       <!-- include image fig-foo [height=4cm] Some caption goes here -->

#     TODO: ideally we should just use normal Markdown syntax, like
#       ![Figure 1](aa.png "basic setup")
#     which comes here as
#       <p><img src="aa.png" alt="Figure 1" title="basic setup" /></p>
#     but since beamer also wants a height spec, we have to figure out how to
#     put that into the Markdown syntax without polluting *that* output

# output is designed to go to pdflatex

# take the preamble from this very program (see the end of the program)
my $preamble = join("", <DATA>);

# read the input text, all in one variable, newlines and all
undef $/;
my $content = <>;

my %metadata=();

$|++;
# get metadata from $content; see example above for what it looks like in the
# actual input file
if ($content =~ s/<!-- begin metadata(.*?)end metadata -->//s) {
    my $metadata = $1;
    for my $i (qw(title shorttitle subtitle occasion shortoccasion date style toctoc tictoc)) {
        $metadata{$i} = $1 if ($metadata =~ /$i *= *(.*?) *$/m);
    }
}

# die "title, occasion not optional" unless $metadata{title} and $metadata{occasion};
# shorttitle defaults to title if not given etc...
$metadata{shorttitle} ||= $metadata{title};
$metadata{shortoccasion} ||= $metadata{occasion};
chomp($metadata{date}  ||= `date -I`);  # perl not brief enough for me ;-)
$metadata{subtitle} ||= "";
$metadata{style} ||= "Darmstadt";

# an "occasion" field should be optional
$preamble =~ s({#date / #occasion})({#date}) unless $metadata{occasion};

# now substitute those in the preamble we pulled in earlier
for my $i (qw(title shorttitle subtitle occasion shortoccasion date style)) {
    $preamble =~ s/#$i/$metadata{$i}/;
}

# if the "tictoc" flag is set...
$preamble =~ s/% (.beamerdefaultoverlayspecification)/$1/ if $metadata{tictoc};

# if the "toctoc" flag is set in the metadata, and the content has any H1 or
# H2 tags, uncomments the parts of the preamble which give you the outline
# pages before every section/subsection
$preamble =~ s/%%#section//g if $content =~ /<h[12]>/ and $metadata{toctoc};

# done with preamble, get it out of the way
print $preamble;

# there's a very specific need in LaTeX/Beamer, if you have "verbatim" (aka
# "code") sections inside a frame, that frame needs to be marked "fragile".
# This subroutine is called within a regex for each frame and if it has a
# verbatim section then [fragile] is inserted.
#
# we also cheat a little by using the same place to undo the HTML entity
# coding (like "&lt;" for "<") that Markdown does in code, because Beamer is
# not expecting HTML entities in LaTeX input :)
sub fragile
{
    my $fc=shift;
    $fc =~ s/{frame}/{frame}[fragile]/
        if ($fc =~ /\\begin{verbatim}/);
    # a bit of cheating here
    $fc =~ s/\\begin{verbatim}.*?\\end{verbatim}/decode_entities($&)/ges
        if ($fc =~ /\\begin{verbatim}/);
    return $fc;
}

for ($content)  # old topic-ing trick to avoid frequent "$content =~"
{
    # latex requires these changes; otherwise they look like metadata to it
    s/\^/\\^{}/g;
    s/\~/\\~{}/g;
    s/\%/\\%{}/g;

    # pause control.  With tictoc, you don't need manually specified pauses,
    # otherwise you do.
    if ($metadata{tictoc}) {
        s/<li>\@\@/<li>/g;
    } else {
        s/<li>\@\@/\\pause\n<li>/g;
    }

    # ok this is a big one; detect and extract inline graphviz...
    while (s/<!-- begin graphviz (\S+)(.*?)end graphviz -->//s) {
        my $figname = $1;
        my $figcode = $2;
        # do something to create the figure from $figcode
        open(DOT, "|dot -Tpng > /tmp/$figname.png") and
        print DOT $figcode and
        close DOT;
    }

    # and use it...
    while (/<!-- include image (\S+) ?(\S.*?)? -->/s) {
        my $out='';
        my $figname=$1;
        my $caption=$2;
        my $matched=$&;
        my $wh = "";        # width/height spec
        $wh = $1 if $caption =~ s/(\[\S+\]) ?//;
        $out .= "\\begin{figure}\n";
        $out .= "\\pgfdeclareimage" . $wh . "{$figname}{/tmp/$figname}\n";
        $out .= "\\pgfuseimage{$figname}\n";
        $out .= "\\caption{$caption}\n" if $caption;
        $out .= "\\end{figure}\n\n";
        # Now you have to make the actual change.  If you attempted to reuse
        # the pattern to make the actual change, you'd have to keep the two in
        # sync if there were any changes later.  So we use $matched
        s/\Q$matched/$out/;
    }

    # delete HTML comments; they confuse Latex/Beamer
    s/<!-- .*? -->//gs;

    # each H3 is a frame, but you have to put an endframe tag just before any
    # succeeding H1/2/3, so we just do that first.  Note that the "end" match
    # could be the start of the next set, so it must be zero-width, otherwise
    # the /g skips past that one on the next round
    s/(<h3>.*?)(?=<h[123]>|$)/$1\n\n\\end{frame}\n\n/gs;

    # H1 is a section, H2 a subsection, H3 a frame
    s/<h1>(.*?)<\/h1>/\\section{$1}/gs;
    s/<h2>(.*?)<\/h2>/\\subsection{$1}/gs;
    s/<h3>(.*?)<\/h3>/\\begin{frame}{$1}/gs;

    # bullet lists; for now we don't support numbered lists
    s/<ul>/\\begin{itemize}/g;
    s/<\/ul>/\\end{itemize}/g;
    s/<li>/\\item\n/g;
    s/<\/li>//g;

    # pre
    s/<pre>(?:<code>)?/\\begin{verbatim}\n/g;
    s/(?:<\/code>)?<\/pre>/\n\\end{verbatim}\n/g;
    # need to add [fragile] to the beginframe -- very fiddly
    our $fc;
    s/\\begin{frame}.*?\\end{frame}/&fragile($&)/ges;

    # em, code, strong
    s/<em>/\\textit{/g;                 s/<\/em>/}/g;
    s/<code>/\\texttt{/g;               s/<\/code>/}/g;
    s/<strong>/\\textbf{/g;             s/<\/strong>/}/g;

    # URLs
    s/<a href="(.*?)">(.*?)<\/a>/\\href{$1}{$2}/gs;

    # does the <p> cause a para break or the </p>?
    s/<\/p>\n\n<p>/\n\\linebreak\n\\linebreak\n/g;
    s/<\/?p>//g;

    print;
}
print "\\end{document}\n";

# main program ended; beamer preamble follows.  Note that this one has my name
# and TCS hardcoded.

__DATA__
    \documentclass{beamer}

    \usecolortheme[RGB={138,43,226}]{structure} 


    \mode<presentation>
    {
      \usetheme{#style}
      \setbeamercovered{transparent}
    }

    \usepackage[english]{babel}
    \usepackage[latin1]{inputenc}
    \usepackage{times}
    \usepackage[T1]{fontenc}
    \title[#shorttitle] % (optional, use only with long paper titles)
    {#title}

    \subtitle
    {#subtitle}

    \definecolor{yahoo}{RGB}{138,43,226}
    \setbeamercolor{yahoo_color}{fg=white,bg=yahoo}

    \author[Kashyap] % (optional, use only with lots of authors)
    {CK Kashyap}

    \institute[Yahoo!] % (optional, but mostly needed)
    {
      Yahoo!\\
      Bangalore}

    \date[#shortoccasion] % (optional)
    {#date / #occasion}

    \subject{Talks}

    \pgfdeclareimage[height=0.74cm]{company-logo}{yahoo}
    \logo{\pgfuseimage{company-logo}}

    %% sita, from page 67 of the beamer userguide
    \setbeamertemplate{navigation symbols}{}

    %% sita, copied from /usr/share/texmf/tex/latex/beamer/themes/outer/beamerouterthemeinfolines.sty
    %% warning, only tested in Darmstadt theme; may not match others
    %% (especially the vertical nav ones like Hannover).  FIXME: figure out how to
    %% disable it when you use themes using vertical navigation
    \defbeamertemplate*{footline}{infolines theme}
    {
      \leavevmode%
      \hbox{%
      \begin{beamercolorbox}[wd=.333333\paperwidth,ht=2.25ex,dp=1ex,center]{author in head/foot}%
        \usebeamerfont{author in head/foot}\insertshortauthor~~(\insertshortinstitute)
      \end{beamercolorbox}%
      \begin{beamercolorbox}[wd=.333333\paperwidth,ht=2.25ex,dp=1ex,center]{title in head/foot}%
        \usebeamerfont{title in head/foot}\insertshorttitle
      \end{beamercolorbox}%
      \begin{beamercolorbox}[wd=.333333\paperwidth,ht=2.25ex,dp=1ex,right]{date in head/foot}%
        \usebeamerfont{date in head/foot}\insertshortdate{}\hspace*{2em}
        \insertframenumber{} / \inserttotalframenumber\hspace*{2ex}
      \end{beamercolorbox}}%
      \vskip0pt%
    }

    % Delete this, if you do not want the table of contents to pop up at
    % the beginning of each subsection:
    %%#section \AtBeginSubsection[]
    %%#section {
    %%#section   \begin{frame}<beamer>{Outline}
    %%#section     \small
    %%#section     \tableofcontents[currentsection,currentsubsection]
    %%#section   \end{frame}
    %%#section }

    % If you wish to uncover everything in a step-wise fashion, uncomment
    % the following command:

    % \beamerdefaultoverlayspecification{<+->}

    \begin{document}

    \begin{frame}
      \titlepage
    \end{frame}

    %%#section \begin{frame}{Outline}
    %%#section   \tableofcontents
    %%#section   % You might wish to add the option [pausesections]
    %%#section \end{frame}

    % \section{sectionname}
    % \subsection[short]{long}
