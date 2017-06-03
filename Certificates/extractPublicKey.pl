#!/usr/bin/perl

use strict;
use warnings;

my$type=$ARGV[0];
my$arg=$ARGV[1];
if($type eq "cert")
{
    system("openssl x509 -pubkey -noout -in $arg");
}
elsif ($type eq "key")
{
    system("openssl rsa -in $arg  -outform PEM -pubout")
}
else
{
    print "Cert or Key?\n";
    exit;
}

