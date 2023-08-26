Rather buggy. My first CP/M DSK image editor / handler. NEVER WORK WITH YOUR ORIGINAL DISK IMAGES.
JUST MAKE A COPY TO THE SAME DIRECTORY THAT THE CPMTOOL.EXE file exists. IT IS NOT DIRECTORY AWARE. 
Always place the disk images in the same folder that the executable is in.

I got a lot wrong, and this program kind of sucks, but many of it's features are still useful. 

Compile with FreeBASIC - eg, Download FREEBASIC and compile with FBC CPMTOOL.BAS and it will make an EXE file or Linux Executable. 

Needs to run under a CMD window, expanded to full-screen 1080P, because there's a LOT of text. 

Download FreeBASIC and locate this BAS file in it's directory where you find the FBC.EXE or FBC64.EXE - 
or where you install it in Linux ( it's tested in Linux and Windows and should work in both. )

It's a command line application as I was playing with that at the time. Type HELP once you get the prompt.

Then you can do things like 

MOUNT FIRST.DSK A:
(Mounts the image FIRST.DSK as A: )
DIR A:
(Shows the directory of the DSK image mounted as A: )
LIST
(Shows the mounted disk images )
SAVE
( saves the currently mounted list of images so they will be automounted next time run ).

Anything you do to a DSK image is INSTANT - if you change it, it is changed.  

A useful feature is BCOPY as it can change disks from one machine to another - eg, Amstrad PCW8256 to PCW9512

use like this;

MOUNT PCW8256.DSK A:
BCOPY A: A: 1

(turns 8256 DISK into a 9512 DISK image )

or

MOUNT PCW9512.DSK B:
BCOPY B: B: 255

(turns 9512 DISK into 8256 DISK image )

You can also use to make a Spectrum +3 disk image bootable etc. 


Here's the inbuilt help menu you get when typing "help"

quit / exit / q         Quit this program.
dir <filename>          Show a directory listing, with file sizes.
dirnum <filename>       Show a directory listing with files numbered
dirhex <filename>       Dump directory allocations in hex.
delete filename         Delete a file of this name. Also accept del.
Examine filename        Show details about a specific file
help                    This help page.
stat                    Show disk status and other variables and information
sector n                Dump sector n as Hex
allocation n            Dump allocation m as Hex.
dib                     Show DSK file Disk Information Block
tib n                   Show DSK file Track Information Block for track n
copy <fname> <fname>    Copy file a to file b. Includes disk identifier. Can copy between disks. Supports default destination filename and wildcards.
bcopy <drive> <drive> n Copy boot sector from first drive to second drive and fudge with the checksum n.
acopy <drv>n1 <drv>n2   Copy allocation file n1 on first drive to n2 on second drive.
scopy <drv>n1 <drv>n2   Copy allocation file n1 on first drive to n2 on second drive.
lcopy <local> <fname>   Copy local drive file to fname Supports default destination filename and wildcards.
lput <fname> <local>    Copy fname to local drive. Supports default destination filename and wildcards.
delete <filename>       Delete file a on currently selected disk
format                  Format currently selected disk. ## Not yet implemented.
fat                     Show File Allocation Table usage. Not that CP/M uses FAT, but well, show allocation usage.
interleave              Show the Interleave of the TIBs throughut the image.
type <filename>         Type out contents of file a
text <filename>         Same as type but filtered for printable ASCII with CRLF permitted.
dump <filename>         Hex Dump file <filename>
typeascii a             Type out contents of file a in ASCII Limited. eg 7 bits, minimum control characters
mount filename drive    Mount image F as drive D eg, mount disk.dsk h: ## Not yet implemented.
unmount drive           Unmount the image assigned to drive d - eg unmount a:
list                    List all mounted disk images.
hextrack n              Hexdump an entire track... Very long... For debugging.
slice n1 n2             Show in hex the current logged disk image file  starting at n1, for n2 bytes.
                         eg, slice &h200 128 would show from hex position &h200 the next 128 bytes. Can use HEX or DEC
rename <fname> <fname>  Rename a file.
renamenum <num>         rename a file by file number - useful when a filename has non-printable characters or control codes.
delnum <num>            Delete file number ( See filename numbers with DIRNUM command. )

Local file functions:
 ldir <filename>         Show the local directory and/or the filename. Supports absolute paths and relative paths.
 lcopy <fname> <fname>   Copy a file from the local host directory to a CP/M image directory.
 lput <fname>            Copy a file from CP/M directory to the local host directory.
 shell <argument>        Perform a shell command.
 save                    Save the current list of mounted disks

ZX related functions:
 tapecopy <localfile>    Copy from the local TAP file to the currently selected disk all files. eg, tapecopy game.tap
 tapedir <localfile>     Show a numbered list of files from the specified local tap file.
 zxlist                  Show a headlerless BASIC file in ZX Tokenised basic as text.
 3list                   Show a +3 formatted BASIC file in ZX Tokenised basic as text.

Well, that's about it. it's a very poorly written program, but I stopped when it did what I wanted. Since there's no other tools out there
that do some of this, I thought I'd make it available nonetheless.
