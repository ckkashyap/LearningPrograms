
use Log::Log4perl;

# Configuration in a string ...
my $conf = q(
		log4perl.category.Kashyap.Rishi          = INFO, Logfile, Screen

		log4perl.appender.Logfile          = Log::Log4perl::Appender::File
		log4perl.appender.Logfile.filename = test.log
		log4perl.appender.Logfile.layout   = Log::Log4perl::Layout::PatternLayout
		log4perl.appender.Logfile.layout.ConversionPattern = [%r] %F %L %m%n

		log4perl.appender.Screen         = Log::Log4perl::Appender::Screen
		log4perl.appender.Screen.stderr  = 0
		log4perl.appender.Screen.layout = Log::Log4perl::Layout::SimpleLayout
	    );

# ... passed as a reference to init()
Log::Log4perl::init( \$conf );


my $logger=Log::Log4perl::get_logger("Kashyap::Rishi");


$logger->info("Hello world");
