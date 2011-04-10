module Resume.Highlights where


import Resume.Type
import Resume.Beautify



highlights :: Type -> Format -> String
highlights Developer format = processHighlights format $ [
	"Filed a patent around image processing based computer UI automation, with a POC implementation. I did POC using VNC and OpenCV",
	"Developed a Virtualization infrastructure using qemu/kvm and a bunch of ruby scripts that allowed folks in my team to quickly generate and work with virtual machines",
	"Very interested in operating system development. Implemented multiple monolithic x86 kernels as hobby projects. Currently fascinated by microkernels and writing EDSLs for writing OS",
	"Have been pursuing functional programming in Haskell",
	"Managed product development teams through multiple major releases and fix-packs",
	"Driven beta programs",
	"Experienced in managing and co-ordinating globally distributed crossfunctional teams",
	"Deployed innovative virtualization solutions with ESX and qemu for productivity enhancement",
	"Have over 10 years of experience in software development with 3 years in people management",
	"Graduated with integrated Masters degree in Management studies from BITS Pilani in the year 2000"
	]


highlights Manager format = highlights Developer format


processHighlights format body = concat items
	where
		items = listify format body
