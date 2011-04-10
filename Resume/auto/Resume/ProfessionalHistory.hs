module Resume.ProfessionalHistory (WorkHistory,WorkAtCompany(..), history) where


data WorkAtCompany = WorkAtCompany { 
		company :: String,
		designation :: String,
		start :: String,
		end :: String,
		description :: String
} deriving (Show)

type WorkHistory = [WorkAtCompany]



history = [
	workAtYahoo,
	workAtIBMMgr,
	workAtIBMDev,
	workAtMicrosoft,
	workAtSun,
	workAtInsilica,
	workAtVirtusaPega,
	workAtVirtusaEMC,
	workAtSeranovaSupport,
	workAtSeranovaSIF

	]

workAtYahoo = WorkAtCompany {
		company = "Yahoo",
		designation = "Principal Engineer - performance analysis and tuning",
		start = "May 2010",
		end = "till date",
		description = "In this role, I'm currently working on exploring the performance of virtualization technologies such as Xen and OpenVZ. Prior to this, I've used Intel's statistical profiling tools to find performance optimization opportunities in a serving side application. I support the team with perl expertise for automations."
	}

workAtIBMMgr = WorkAtCompany {
		company = "IBM India Software Labs",
		designation = "Engineering Manager",
		start = "July 2007",
		end = "May 2010",
		description = "As an engineering manager, I performance managed a team of 12 engineers. At IBM, this meant representing the team members in the team based decision making process twice a year. The role also involved representing the development team in the cross-functional release management meetings. I've also driven beta programs."
	}


workAtIBMDev = WorkAtCompany {
		company = "IBM India Software Labs",
		designation = "Staff software engineer",
		start = "December 2005",
		end = "July 2007",
		description = "I started out by leading a two member team that worked on Linux. Once the Linux issues were stabilized, I was moved to a larger role of leading the core modules of the product. This involved, hands on work and also mentoring a group of four members. This product is based on a couple of IDEs one which is eclipse based and the other which is Visual studio based. There are some native components that are in C++."
	}


workAtMicrosoft = WorkAtCompany {
		company = "Microsoft India Development Center, Hyderabad",
		designation = "Software design engineer",
		start = "March 2005",
		end = "December 2005",
		description = "I worked on two products - Virtual PC and Virtual Server. I worked on two fixpack releases of Virtual PC and the release of Virtual Server 2005."
	}

workAtSun = WorkAtCompany {
		company = "Sun Microsystems",
		designation = "Staff software engineer",
		start = "September 2004",
		end = "March 2005",
		description = "As a part of this team I was responsible to fix bugs reported on the Sun’s Webserver. Also, I was in charge of setting automated GAT(General Acceptance Test). Automated GAT involved writing Perl script to continuously check out the source, do a build and run the test suite. Since the repository was in a remote location, I had implemented a kind of ”double buffer” to speed up the process - I used to start the checkout of the source in a parallel location when the test was running on the checked out location. During the course of work here, I had opportunity to understand PKI."
	}

workAtInsilica = WorkAtCompany {
		company = "Insilica Semiconductors",
		designation = "Senior software engineer",
		start = "October 2003",
		end = "September 2004",
		description = "The goal of this project was to implement the 802.11 protocol stack. During the development of the stack, the wireless hardware was still being fabricated. Therefore, ethernet was used to emulate the physical layer. Also, an extensive test-suite was designed and developed in perl to test the stack from a central controller."
	}


workAtVirtusaPega = WorkAtCompany {
		company = "Virtusa",
		designation = "Software Engineer",
		start = "March 2003",
		end = "October 2003",
		description = "This project involved writing an expression compiler for the Pega system’s rules engine. ANTLR (lex/yacc equivalent in the java world) was used for this purpose. Developing the front end of this project involved extensive use of java-script to dynamically make HTTP connections to the server and parsing the retrieved XMLs."
	}

workAtVirtusaEMC = WorkAtCompany {
		company = "Virtusa",
		designation = "Software Engineer",
		start = "March 2002",
		end = "February 2003",
		description = "The goal of the EMC EDM Linux Port project was to port EDM (EMC's Data Manager)from Solaris to Linux. EDM contains 2.5 million lines of C and C++ programs. The UI part of EDM was in Java. The project was carried out in multiple phases. The first phase involved replacing the Solaris specific calls in the application source base with the POSIX equivalent. The second phase involved compiling the whole thing on Linux. The third phase involved executing the application and debugging on both the platforms."
	}


workAtSeranovaSupport = WorkAtCompany {
		company = "Seranova",
		designation = "Software Engineer",
		start = "November 2001",
		end = "March 2002",
		description = "This project involved doing night shifts and waiting for client calls and resolving issues with SIF."
	}

workAtSeranovaSIF = WorkAtCompany {
		company = "Seranova",
		designation = "Software Engineer",
		start = "July 2000",
		end = "October 2001",
		description = "Standard Implementation Framework (SIF) is the web contentmanagement utility developed for a fortune 500 American client. This utility was developed using Vignette’s Story-Server. Using the tool a person with no knowledge of HTML could create and update webcontent of the corporate web site. The tool allowed creation of page layouts on the fly. This was achieved using a JAVA applet. All the information about the layout and web contents were stored using XML.  I was a developer in one of the modules. My training period was till December 2000."
		
	}
