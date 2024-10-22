Whitaker's Words
================
a.k.a. Latin WORDS
------------------
a Latin<->English dictionary program and inflection parser

Col. William Whitaker was the chairperson of the Department of Defense working group that established the Ada language.

Whitaker was also an amateur classicist.  Around 1993, he started a dictionary, written in Ada, to assist his studies.  He called the dictionary "Latin WORDS" or just "WORDS".

Whitaker spent years revising, expanding, and correcting the dictionary, frequently engaging with scholars, students, and other correspondents in a sort of pre-Wiki era collaboration.

Col. Whitaker continued working on WORDS until c. 2006.  By then, Whitaker's dictionary was one of the most comprehensive in the world.

The materials in this repository further build on Whitaker's efforts.


**New features, easier-to-read output options, expanded dictionary, and bugfixes**

*New for users*

Based on Whitaker's last version of Words (1.97FC), with
- an *expanded and corrected dictionary* (including over 200 new main dictionary entries)
- *output formatted* using vt100/ANSI codes to enhance readability on consoles (optional; see below)
- more *consistent and better sorted output* (eliminated various situations where the program returned blank lines, duplicate results, incorrectly organized results, or inconsistently formatted results)
- *clarified* a handful of incomplete or ambiguous *grammar notes*
- *improved inflections and "tricks"* procedures (changes are conservative and were verified with the Oxford Latin Dictionary)
- a few minor *bug fixes* (most of which have been accepted into @mk270's repository -- see below)
- the ability to *accept input with macrons* (and other accented Unicode characters)
- an optional module to *convert digits to Roman Numerals*
- an *online help system*, including a reference for the sometimes cryptic abbreviations and "dictionary codes" that Words can provide
- comprehensive *off-line documentation of WORDS*'s data files and the codes, abbreviations, and conventions they use; see the Open Document Format (ODF) spreadsheet in the docs directory
- *more flexibility as to where the data files are stored* so that, e.g., the executable and data can be separated into the traditional Unix-like directory structure (the program first checks for data in the working directory, then checks whether a LATINWORDS environment variable points to the data files, and finally checks the system PATH variable)
- a different default change-language character ("_") for *better interoperability with POSIX shells* when using English->Latin translation from the command line (previous versions of Words defaulted to "~" as the change-language character)
- *new command-line options* to disable potentially problematic behavior (e.g., one traditionally exits words by entering two blank lines, but that can cause the program to terminate when copy-pasting blocks of text into the terminal) and to disable specific translation features (useful for pedagogical purposes and lab environments)


*New for developers and administrators*

- *Easier dictionary rebuilds.*  The dictionary-building programs have been modified to accept command line options so that dictionary rebuilds can be done non-interactively.  A platform-independent command script has been provided to automate the entire process (_rebuild.sh_ in the _tools_ directory).  And the dictionary-rebuild documentation has been greatly simplified.
- *New notice and warnings for logging user changes to files* OTHER THAN the built-in WORD.OUT, WORD.UNK, WORD.MOD, and WORD.MDEV.  When output is written to an arbitrary file name (i.e., when Words is run with parameters <arbitrary input file name> <arbitrary output file name>), a notice goes to STDOUT.  As for local dictionary updates, if running interactively with output re-directed to file, a warning goes to the output file; if running interactively with output going to STDOUT, the warning goes to STDERR.  If Pearse codes are enabled, the new notice and warnings are tagged with the new Pearse code "07."
- Despite the above changes, *no script or output-parsing changes should be required* because (i) The new command-line options do not interfere with Words's traditional command-line operation; (ii) no change to pre-existing Pearse codes (codes that identify the type of content in each line of output to make it easier to process Words's output via script or other automated tool), and the newly added 07 Pearse code arises in rare user-interactive sessions; and (iii) when vt100/ANSI formatting is turned off there should be no difference in line format output vs. Words 1.97FC with a single exception--the line and word number are no longer printed next to "===== UNKNOWN" when using WRITE_OUTPUT_TO_FILE and WRITE_UNKNOWNS_TO_FILE (that behavior was part of the STATS functionality, which was removed because it was broken and obscure).
- *WORDS can still easily be configured as a listening service* that silently receives input and returns output to an arbitrary file or pipe if you build with SUPPRESS_PREFACE = TRUE in config.ads.


Why?
----
*Words should be continued and expanded*

Words is an outstanding piece of software. It's a comprehensive dictionary and a pretty-darn-accurate grammar parser.  Now almost 30 years old, WORDS has helped generations of Latin students.

*I've had dictionary updates and notes sitting around for a long time*

Back in the 90s I did the earliest OS/2 ports of Words and began corresponding with Whitaker about dictionary and grammatical issues.  In the early 2000s I packaged and distributed graphical front-ends to WORDS for Mac OS (Classic) and MacOS X.  (One of my collaborators kept it up and did much nicer front ends after I stopped!)

Through all that I kept my own WORDS codebase.  I also had dictionary updates and notes re entries and output that could use touching up.

Implementing my notes on the dictionary and finishing work on the features I always wanted became one of my "Covid projects."


Why trust your dictionary and grammar changes?
-------------------------------------------------------
Judge for yourself.  Review the diffs of DICTLINE.GEN (the main dictionary file) and the .LAT files (the inflections, addons, and uniques files).  They're easy to understand.

Other than that, you'll have to trust me, a random person on the internet.  I'm going to preserve some privacy and not post my CV, but I care about this kind of stuff and take it seriously.  FWIW, I studied Latin for a very long time, I've got a fancy education, and my career requires me to pay careful attention to words and grammar pretty much every day.  Take it or leave it :)


What about @mk270's project?
------------------------------------
@mk270 started modernizing and improving Words's coding standards.  I've submitted bugfixes to @mk270's fork, and I will continue to try to make helpful contributions.

However, I've publicly released my version because:

_First_, @mk270's project is out of date and may be at risk of abandonment.  The repo hasn't been updated in about two years, and its project files use flags that will make the build fail by default under recent versions of GNAT.

_Second_, I hope others will find my quality-of-life improvements helpful.  I certainly do; they're changes that I wanted during my years as a Latin student and regular WORDS user.

It would be presumptuous in the extreme if I dumped the changes I made over the years--changes to the code and dictionary alike--onto someone else's project.  If others think my changes or new features and useful, that's excellent.  Maybe they will be adopted in some form.  I'll help port them over.

_Third_, and closely related, I'm more interested in improving the dictionary and grammar parsing than improving code quality.

WORDS's real strength is pointing you down the right path when you're stuck on a rare word or form.  Accordingly, I make different trade offs between modernizing code vs. inadvertently changing WORDS's output in rare corner cases.  Wherever possible, I leave Words's existing routines alone, even if that means tacking on another sub-routine to handle a situation that could only arise with a couple forms of one or two words.

The rationale is a bit philosophical, and reasonable people will disagree with me, but here's my thinking:

Whitaker's code is procedural and has lots of complex branches, but it's well tested and generates accurate results.  Decades of use make make Words's many conditionals and branches fit for this purpose.

To a great extent, this complexity is necessary for algorithmic natural-language processing.  Language and grammar are inconsistent and often illogical, making them a bad fit for programming languages except DSLs every bit as complex as WORDS (and, perhaps, so-called "AI" models that are much more complicated than WORDS).

Thus, there's an inherent tension between WORDS's goal and programmatic attempts to simplify it or conform it to, e.g., OOP practices.  Similarly, if we re-wrote WORDS to use more side-effect-free functions, we'd need to overhaul WORDS's data handling and then chain together so many single-use-only functions that we'd end up with code no easier to understand than the existing if-then branches.

I'd rather stick with messy code that produces known results.


_Fourth_, keeping WORDS's old-school style may ultimately preserve it better than modernizing it.  Whitaker used only one(!) Ada 95 package (Ada.Command_Line).  Everything else was valid Ada 83 code.  This lowest-common-denominator approach contributed to WORDS's longevity.  Whitaker's code still compiles on a plain-vanilla installation of the latest GNAT installation.  There's no need for a complicated project file, autoconfig, or make file.  And hacking on WORDS doesn't require familiarity with Ada's more sophisticated features.

At the same time, I'm practical, not ideological, about deploying new standard packages.  For example, Ada 2013's Unicode packages make it easy for WORDS accept input that contains macrons, and Ada 2005 packages make it easy to put the executable and data files in separate directories.     Documentation in the docs directory explains how to remove all dependencies on Ada features introduced after Ada 95 (and Ada 83, too).

We don't know whether the "modernizing" or "keep it basic"  approach will do better in the end, so let's do both.


Rights and license
------------------

Whitaker developed and released Words under a 1990s-style public domain license, declaring Words "free for any use" or "free for your use."

He also made the following statements in the Words documentation:

- "LICENSE: [ . . . Words is] freely available for anyone to use for any purpose. It may be converted to other languages, used in pieces, or modified in any way without further permission or notification."
- "The program source (in Ada) and dictionary are freely available for rehosting."

Unfortunately, this has caused some confusion because these words are not a facially obvious match for today's well known OSS form licenses.  There's a discussion at https://github.com/mk270/whitakers-words/issues/118

In fact, the licensing situation is straightforward:

The BSD 2-clause license accurately captures Whitaker's intent on the issues that matter:  First, Words is free for any use, commercial or non-commercial.  Second, derivative works need not be open sourced.

There is no conflict between Whitaker's license and licensing post-Whitaker contributions and derivatives under the BSD 2-clause license.  If anything, Whitaker's license is less restrictive than the BSD 2-clause license.

Therefore, compliance with the BSD 2-clause license necessarily implies compliance with Whitaker's license:

*LICENSE:*

Portions copyright (c) 1993-2024 William Armstrong Whitaker and subsequent contributors.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
