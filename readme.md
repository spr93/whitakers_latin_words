Whitaker's Words
================
a.k.a. Latin WORDS
------------------
a Latin<->English dictionary program and inflection parser

**With new features, expanded dictionary, and bugfixes**

Ready-to-use binaries in /release -- a lot of them:
macOS (amd_64), Linux (amd_64), Windows (amd_64 and Win32), FreeBSD (amd_64), and OS/2 (emx 0.9d or higher)


*What's this?*

Col. Whitaker's last version of Words (1.97FC), with
- an expanded and corrected dictionary (110 new stems and 6 new uniques)
- output formatted with vt100 codes to enhance readability (optional; see below)
- more consistent list output (no known situations where the program returns duplicate results or )
- a comprehensive online help system, including a reference abbreviation and code definitions
- conversion to Roman Numerals (optional module)
- clarified grammar notes
- lightly modified inflections and "tricks" (conservative and verified with the latest Oxford Latin Dictionary)
- bug fixes throughout
- more flexibility as to where the data files are stored (first checks the working directory, then checks whether a LATINWORDS environment variable contains the directory information, then checks the system PATH variable)--so that, e.g., the executable and data can be separated into the traditional Unix-like directory structure
- also in the vein of being more Unix-like, underscore ('_') and not tilde ('~') (the traditional abbreviation for the user's home directory) is the default change-language character (no more remembering to put quotes around the change-language character when running Words from a shell command line)
- command-line options to disable features (useful for pedagogical purposes and lab environments, but these are *not* robust and cannot, by themselves, be relied upon for any real security)

Fully compatible with Col. Whitaker's last version:
- No change to Pearse codes
- When vt100 formatting is turned off there is no difference in line output
- The new command-line options do not interfere with Words's traditional command-line directives


*Anything else?*

Yes:
- User friendly documentation of the Word data files and the codes, abbreviations, and conventions they use; see the Open Document spreadsheet in the docs directory
- The dictionary-building programs have been modified to accept command line options so that dictionary rebuilds can be done non-interactively; see rebuild.sh in the tools directory
- Simplified dictionary rebuild instructions
- Updated "DICTPAGE" output (a dictionary listing, formatted like a traditional paper dictionary, in HTML) is in /dictionary

Why?
----
*Words should be continued and expanded*

Words is amazing. It's a comprehensive dictionary and a pretty-darn-accurate grammar parser.  Now almost 30 years old, Words has helped generations of Latin students.

Words was a labor of love for Col. Whitaker, the chair of the Department of Defense working group that created the Ada standard.  He worked on the program from at least 1993-2006.  He spent a huge amount of effort refining its results and handling corner cases.  He corresponded with people all of the world and used their input to make Words better.  

=> I've had dictionary updates and notes sitting around my hard drive for a long time

Back in the 90s I did the earliest OS/2 ports of Words and began corresponding with Col. Whitaker about dictionary and grammatical issues.  In the early 2000s I packaged and distributed graphical front-ends to Words for Mac OS (Classic) and MacOS X.  (One of my collaborators kept it up and did much nicer front ends after I stopped!)

Through all that kept my own Words codebase for decades.  I also had dictionary updates and notes re entries and output that could use touching up.  That stuff mostly sat on my backup disks, but every once in a while I'd get the urge to poke at the code.  Finishing work on the features I always wanted has been one of my main "covid projects."


What about @mk270's awesome project?
------------------------------------
It's great!  @mk270 is doing the hard work of modernizing and improving Words's coding standard, both of which are critical if Col. Whitaker's work is to be preserved and improved for the _next_ 30 years.  I've submitted bugfixes to @mk270's fork, and I will continue to try to make helpful contributions to open issues.

The fork in this repo is a little different.  It aims to add features and make Words the program that I personally wanted when I was a Latin student who used Words regularly.  It would be presumptuous in the extreme if I dumped the changes I made over the years--changes to the code and dictionary alike--onto someone else's project ("hi, internet rando here, just trying to push a bunch of changes that I made to a 15-year-old version of the code base to your repo.  also, here are new features no one asked for.").  If others think my changes or new features and useful, that's excellent.  Maybe they will be adopted in some form.  I'll help the porting.

I also want to avoid introducing new issues in the short term.  Col. Whitaker's code is messy in places, but it's well tested and generates accurate results.


*License:*

Col. Whitaker released his works under his own permissive license (see /docs/"Whitaker's original"/words.html for his license information).  All copyrightable expression in my contributions is subject to those same terms.
