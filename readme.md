Whitaker's Words
================
a.k.a. Latin WORDS
------------------
a Latin<->English dictionary program and inflection parser

The chairperson of the Department of Defense working group that established the Ada language, Col. William Whitaker, was an amateur classicist and a fan of Caesar's military writings.  Around 1993, he started a dictionary, written in Ada, to assist in reading Caesar.  He maintained and expanded that dictionary until his death around 2006.

By that time, it was one of the largest Latin dictionaries in the world.  Whitaker spent years revising, expanding, and correcting the dictionary, frequently engaging with scholars, students, and other correspondents in a sort of pre-Wiki era collaboration.

The code in this repository further builds on Whitaker's efforts.


**New features, easier-to-read output options, expanded dictionary, and bugfixes**

*New for users*

Based on Whitaker's last version of Words (1.97FC), with
- an expanded and corrected dictionary (including over 200 new main dictionary entries)
- output formatted using vt100/ANSI codes to enhance readability on consoles (optional; see below)
- more consistent and better sorted list output (eliminated various situations where the program returned blank lines, duplicate results, incorrectly organized results, or inconsistently formatted results)
- clarified a handful of incomplete or ambiguous grammar notes
- lightly modified inflections and "tricks" procedures (changes are conservative and were verified with the Oxford Latin Dictionary)
- a few minor bug fixes (most of which have been accepted into @mk270's repository -- see below)
- the ability to accept input with macrons (and other accented Unicode characters) *This uses Ada 2022 features and is therefore in an optional module; everything else complies is vanilla Ada 2005.*
- an optional module to convert digits to Roman Numerals
- an online help system, including a reference for the sometimes cryptic abbreviations and "dictionary codes" that Words can provide
- comprehensive off-line documentation of the Word data files and the codes, abbreviations, and conventions they use; see the Open Document Format (ODF) spreadsheet in the docs directory
- more flexibility as to where the data files are stored so that, e.g., the executable and data can be separated into the traditional Unix-like directory structure (the program first checks for data in the working directory, then checks whether a LATINWORDS environment variable points to the data files, and finally checks the system PATH variable)
- a different default change-language character ("_") for better interoperability with POSIX shells (previous versions of Words defaulted to "~" as the change-language character)
- command-line options to disable potentially problematic behavior (e.g., one traditionally exits words by entering two blank lines, but that can cause the program to terminate when copy-pasting blocks of text into the terminal) and to disable specific translation features (useful for pedagogical purposes and lab environments)


*New for developers and administrators*

- Easier dictionary rebuilds.  The dictionary-building programs have been modified to accept command line options so that dictionary rebuilds can be done non-interactively.  A platform-independent command script has been provided to automate the entire process (see rebuild.sh in the tools directory).  And the dictionary-rebuild documentation has been greatly simplified.
- New notice and warnings for logging user changes to files OTHER THAN the built-in WORD.OUT, WORD.UNK, WORD.MOD, and WORD.MDEV.  When output is written to an arbitrary file name (i.e., when Words is run with parameters <arbitrary input file name> <arbitrary output file name>), a notice goes to STDOUT.  As for local dictionary updates, if running interactively with output re-directed to file, a warning goes to the output file; if running interactively with output going to STDOUT, the warning goes to STDERR.  If Pearse codes are enabled, the new notice and warnings are tagged with the new Pearse code "07."
- _Despite the above changes, no script or output-parsing changes should be required:_  (i) The new command-line options do not interfere with Words's traditional command-line operation; (ii) no change to pre-existing Pearse codes (codes that identify the type of content in each line of output to make it easier to process Words's output via script or other automated tool), and the newly added 07 Pearse code arises in rare user-interactive sessions; and (iii) when vt100/ANSI formatting is turned off there should be no difference in line format output vs. Words 1.97FC with a single exception--the line and word number are no longer printed next to "===== UNKNOWN" when using WRITE_OUTPUT_TO_FILE and WRITE_UNKNOWNS_TO_FILE (that behavior was part of the STATS functionality, which was removed because it was broken and obscure)
- Words can still easily be configured as a listening service that silently receives input and returns output to an arbitrary file or pipe (build with SUPPRESS_PREFACE = TRUE in config.ads)


Why?
----
*Words should be continued and expanded*

Words is an outstanding piece of software. It's a comprehensive dictionary and a pretty-darn-accurate grammar parser.  Now almost 30 years old, Words has helped generations of Latin students.

Words was a labor of love for Whitaker, the chair of the Department of Defense working group that created the Ada standard.  He worked on the program from at least 1993-2006.  Over that time, he spent a huge amount of effort refining its results and handling corner cases.  He corresponded with people all of the world and used their input to make Words better.

*I've had dictionary updates and notes sitting around my hard drive for a long time*

Back in the 90s I did the earliest OS/2 ports of Words and began corresponding with Whitaker about dictionary and grammatical issues.  In the early 2000s I packaged and distributed graphical front-ends to Words for Mac OS (Classic) and MacOS X.  (One of my collaborators kept it up and did much nicer front ends after I stopped!)

Through all that I kept my own Words codebase.  I also had dictionary updates and notes re entries and output that could use touching up.

Implementing my notes on the dictionary and finishing work on the features I always wanted was one of my main "covid projects."


What about @mk270's awesome project?
------------------------------------
It's great!  @mk270 is doing the hard work of modernizing and improving Words's coding standards.  That's critical if  Whitaker's work is to be preserved and improved for the _next_ 30 years.  I've submitted bugfixes to @mk270's fork, and I will continue to try to make helpful contributions to open issues.

The fork in this repo is a little different.

_First_, it aims to add features and make Words the program that I personally wanted when I was a Latin student who used Words regularly.  This includes dictionary and grammatical enhancements, new features, and changes to the output.

It would be presumptuous in the extreme if I dumped the changes I made over the years--changes to the code and dictionary alike--onto someone else's project ("hi, internet rando here, just trying to push a bunch of changes that I made to a 15-year-old version of the code base to your repo.  also, here are new features no one asked for.").  If others think my changes or new features and useful, that's excellent.  Maybe they will be adopted in some form.  I'll help the porting.

_Second_, I'm more interested in improving the dictionary and grammar than improving code quality.  Words's real strength is pointing you down the right path when you're stuck on a rare word or form that appears totally inscrutable.

Accordingly, I make different trade offs between modernizing code vs. inadvertently changing Words's output in rare corner cases.  Wherever possible, I leave Words's existing routines alone, even if that means tacking on another sub-routine to handle a situation that could only arise with a couple forms of one or two words.

The rationale is a bit philosophical, and reasonable people will disagree with me, but here's my thinking:

Whitaker's code is procedural and has lots of complex branches, but it's well tested and generates accurate results.

To a great extent, this complexity is necessary for natural-language processing.  Language and grammar are inconsistent and often illogical; not a good fit for programming languages except DSLs every bit as complex as Words (and, perhaps, AI algos that are much more complicated than Words).  Words's value lies in how it handles the arbitrariness of natural-language.  That is, how Words processes "rare" corner cases that can arise only under a narrow combination of conditions and inputs.  Decades of use make make Words's many conditionals and branches fit for this purpose.

Thus, there's an inherent tension between Words's goal and programmatic attempts to simplify it or conform it to, e.g., OOP practices.  Similarly, if we re-wrote it to use more side-effect-free functions, we'd need to overhaul Words's data formats and then chain together so many single-use-only functions that we'd end up with code no clearer or cleaner than the existing if-then branches.

I'd rather stick with inelegant code that produces known results.

For these reasons, my changes don't go out of their way to make the Words code more maintainable (some changes surely do the opposite).  On the other hand, I've avoided changes that could disrupt Words's existing quirks and opaque string manipulations.  There's magic in those obscure routines, and I'm okay with that, so long as the results are right.

...and yet, my changes result in slightly better performance, at least by an imperfect benchmark (as all benchmarks are).  Here's how long it takes various versions of Words to translate the Aeneid on my 2019 Intel MacBook Pro (using "time words aeneid.txt out.txt," where the program was been compiled with a 2021 GNAT build using -O3, and taking the numbers from the second consecutive run to reduce the effect of caching):
Whitaker's 1.97FC
mk270's refactor (as of August 2022)
This repo (as of August 2022)

_Third_, platform independence.  Whitaker used only one(!) Ada 95 library (Ada.Command_Line).  Everything else was valid Ada 83 code.  This lowest-common-denominator approach contributed to Words's longevity-it does not rely on any deprecated features, nor does it rely on any implementation-specific libraries or unusual behavior that exists only in ancient or particular compilers.  There's no need for a complicated project file, autoconfig, or make file.  And hacking on Words doesn't require familiarity with Ada's more sophisticated features.

There are exceptions, of course, because some functions are simple and highly portable using newer Ada standards (command-line options, data file location flexibility, and Unicode processing).

In Whitaker's tradition, the docs directory contains instructions for how to modify or strip the features necessary in order to compile Words on just about any compiler.  It'll take you just a couple minutes to conform the code to the Ada 95 standard.

Why should I trust your dictionary and grammar changes?
-------------------------------------------------------
Review the diffs of DICTLINE.GEN (the main dictionary file) and the .LAT files (the inflections, addons, and uniques files) for yourself.  They're easy to understand.

Other than that, you'll have to trust me, a random person on the internet.  I'm going to preserve some privacy and not post my CV, but I care about this kind of stuff and take it seriously.  FWIW, I studied Latin for a very long time, and I've got a fancy education (Stanford, post-graduate degree) and career in fields that have required me to pay careful attention to words and grammar pretty much every day.  Take it or leave it :)

*Rights and license*

Whitaker developed and released Words under a 1990s-style public domain license, declaring Words "free for [any | your] use."

He also made the following statements in the Words documentation:

- "LICENSE: [ . . . Words is] freely available for anyone to use for any purpose. It may be converted to other languages, used in pieces, or modified in any way without further permission or notification."
- "The program source (in Ada) and dictionary are freely available for rehosting."
See the documentation in in the docs/Whitaker's original directory.

Unfortunately, this has caused some confusion because these words are not a facially obvious match for one of today's well known OSS form licenses.  See discussion at https://github.com/mk270/whitakers-words/issues/118

The licensing situation is actually straightforward:

The BSD 2-clause license accurately captures Whitaker's intent on the issues that we worry about today:  First, Words is free for any use, without regard to whether that use is commercial or non-commercial use.  Second, derivative works need not be open sourced.

There is no conflict between Whitaker's license and licensing post-Whitaker contributions and derivatives under the BSD 2-clause license.  If anything, Whitaker's license is less restrictive than the BSD 2-clause license.

Therefore, compliance with the BSD 2-clause license necessarily implies compliance with Whitaker's license.

In short, the most restrictive possible license conditions can be expressed in standard OSS license form as follows:

LICENSE:

Portions copyright (c) 1993-2022 William Armstrong Whitaker and subsequent contributors.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
