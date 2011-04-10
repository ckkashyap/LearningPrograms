module Resume.Summary (summary) where

import Resume.Type
import Resume.Beautify

summary :: Type -> Format -> String

summary Developer  format = processSummary format $ [
	"Computer programming has been my hobby since school days. I started with BASIC programming in 1990. I was fascinated with DOS games and in my quest to write fast games, I moved from BASIC to assembly to C. On the way, I got hooked on to protected mode and eventually operating system kernel programming. I have a few projects registered with github (and earlier google projects) ; some of the interesting ones are, a port of minix3 microkernel so that it builds on Linux and a Ruby based VNC automation engine. More recently, Functional Programming has caught my fancy and I have been exploring Haskell for over a year now. As a learning exercise, I'm working on a VNC based platform independent interactive graphics rendering library using Haskell ; https://github.com/ckkashyap/Chitra",
	"Professionally, I started my career with Seranova where I worked on Java based internet solutions. I switched jobs in the earlier part of my career mainly to get involved with embedded systems. In the later part, it was for looking for larger roles where I could contribute more with my deep understanding of software and exposure to a wide range of it. I am now looking for a job where I can use my computer and management skills to explore and solve larger and challenging problems."
	]

summary Manager format = processSummary format $ [
	"I have over 10 years of industrial experience in developing software ranging from device drivers to web applications. My experience includes 3 years of people management at IBM India Software Labs and driving multiple releases via a geographically distributed cross functional team. I graduated from BITS Pilani in 2000 with MMS - Master of Management Studies. MMS used to be an integrated masters degree which was equivalent of BE + MBA.  While at BITS, I was part of the robotics lab and I did a semester long internship at Center for Artificial Intelligence and Robotics, Bangalore.",
	"Computer programming has been my hobby since school days. I started with BASIC programming in 1990. I was fascinated with DOS games and in my quest to write fast games, I moved from BASIC to assembly and C. On the way, I got hooked on to Intel's protected mode and eventually operating system kernel programming. I have a few projects regis- tered with github (and earlier google projects) ; some of the interesting ones are, a port of minix3 microkernel so that it builds on Linux and a Ruby based VNC automation engine. More recently, Functional Programming has caught my fancy and I have been exploring Haskell for over two years now. I believe that functional programming is one of the ways of harnessing the multicore era. As a learning exercise, I'm working on a VNC based platform independent interactive graphics rendering library using Haskell ; https://github.com/ckkashyap/Chitra",
	"Professionally, I started my career with Seranova where I worked on Java based internet solutions. I switched jobs in the earlier part of my career mainly to get involved with embedded systems. In the later part, it was for looking for larger roles where I could contribute more with my deep understanding of software and exposure to a wide range of it. I am looking for a job where I can use my computer and management skills to solve larger problems; help people with leveraging various kinds of automations. In my earlier jobs, I've taken up doing tutorial classes a number of times and have received very positive feedback; I've also enjoyed such sessions a lot. It'll be great if such activities become part of my job portfolio."
	]


processSummary format body = concat (map (makeParagraphs format)  body)
