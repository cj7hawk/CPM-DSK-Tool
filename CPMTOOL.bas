
rem ZX Spectrum .DSK core program. Will build disks of 180, 360 and 720K but probably I might not go beyond 180K since that is enough for file transfer... OTOH, I really want a way to build a large number of CP/M files onto a large disk.
rem Should work like a DOS command - eg, cdir, ccopy, cdelete, crename, cdata (stats ) and cpack and cunpack... ( for moving a LOT of files in a single batch ).
rem - Started programming this on 11-12-2021
rem - Native to Freebasic. Download Freebasic, copy this .BAS file to the freebasic directory and compile to an executable with "fbc supercart.bas" to create the executable.

rem Lexical Functions



dim shared CLI as string        : Rem input from virtual command line.
dim shared POSTCLI as string    : Rem Input from virtual CLI when there is additional text after the cursor position.
dim shared HISTORY(30) as string : Rem Command line history is 30 lines long
dim shared walk as integer      : Rem remember our place in history.
dim shared archaic as integer : archaic=30      : rem Record the maximum length of history. ( whatever DIM HISTORY is set to ).
dim shared KEY as string        : rem Single keypress.
dim shared CLIFUNC as string    : rem Function created by CLI - The "Command" whether local or remote.
dim shared drive as integer     : rem What drive are we talking about?
dim shared loggeddrive as integer       : rem Logged drive when we temporarily change drive (eg, DIR, COPY etc )
dim shared sfile as string      : rem When we know what file we are looking for, and just want the single filename temporarily.
dim shared dspec as string      : rem Drive Specifier - When removed from the file string.

dim shared defaultdrive as integer      : rem What is the default drive? In case it's not specified - I need to get rid of logged drive and have a way to change the default.
dim shared defaultfname as string               : rem What is the current default filename if not specified ( eg, copy a:*.* b: Needs to copy the currently matched filename )
dim shared defaultresource as string    : rem Stuff like A: B: 3: etc. Could also be a network address in the future.
dim shared defaultdsk as string                 : rem What is the filename of the DSK image of the CURRENT operation (eg, Last Opened )
rem dim shared drivenum as integer                      : rem Drive number of the CURRENT operation, whether default or otherwise.


rem TEST functions.

DIM shared debug as integer


rem DISK functions

DIM shared DISK as string                               : rem I need a million characters ( 8 bit characters ) to hold an entire DISK. I can cut it up with string commands and functions.
DIM shared DISKTRACK as string                  : rem Sometimes I just want a piece of the DISK.
DIM shared DISKSECTOR as string                 : rem And sometimes just a sector.
DIM shared DISKALLOCATION as string             : rem and sometimes the allocation.
DIM shared WFILE as string                              : rem Write File Name as a string. For saving the image back to hard disk when we've finished modifying it.
DIM shared comm as string                               : rem Command ( first argument ).
DIM shared CONFIG as string                             : rem Incoming Configuration File.
DIM shared DSKFILE(16) as string                : rem All the disk files we have mounted from the config.
DIM shared FILE as string                               : rem A SINGLE FILE... Read from or written to a disk.
DIM shared FILEENTRY as string                  : rem Somewhere to copy the file directory entry to once we find a file.
DIM shared BLOCK as string                              : rem General - for when we just want a block of data...
DIM shared XDPB as string                                               : rem Current Disk Extended Disk Parameter Block.
DIM shared LDIR as string                       : rem Local Directory from current launch directory. Can use .. if necessary.

DIM shared AMDefault as integer                 : rem Is it a default Amstrad disk?

DIM shared FILES(256) as string                 : rem Make up an array of 256 strings for filenames as we build the directory.
DIM shared FILESIZES(256) as integer    : rem File sizes of each file.
rem DIM shared FAT(360) as integer                  : rem FAT can contain the filename for each cluster, as well as building a map of the file tables.
DIM shared FAT(65536) as integer                  : rem FAT can contain the filename for each cluster, as well as building a map of the file tables.
DIM shared SINTERLEAVE(255) as integer  : rem Interleave Tables. If required ( for out of order sectors ).
DIM shared SINTERLEAVED as integer              : rem If Interleave is 1, then sectors are interleaved.
DIM shared ROOT as integer                              : rem ROOT directory cluster... Initially 0... But I'm going to add directories later.
DIM shared DIRCOUNT as integer                  : rem  Directory Counter... Count through the files.
DIM shared PATH as string                               : rem The path other than the root. \imagename.img\directory\filename.txt   Can copy between images.  Should be stateless.
DIM shared segment as integer                   : rem We need to know what segment of a file we found since a file can have multiple entries if over 16K.
DIM shared fileend as integer                   : rem How much of the last file allocation is part of that file?
DIM shared Numfiles as Integer                  : rem How many files in a directory ( starts with 0 ).
DIM shared Filepos as Integer                   : rem Filepos is the relative position of allocations under a particular filename that a filename in a directory contains.
DIM shared NOTFOUND as integer                  : rem In case file is not found, will be 1.... Otherwise make 0
DIM shared DirSearch as integer                 : rem counter as we search the directory for a file match.
DIM shared Extent as integer                    : rem Calculate the extent of a file
DIM shared Priorextent as Integer               : rem Might need to compare two extents.
DIM shared FileLength as integer                : rem Calculate the Length Of File in CP/M
DIM shared DISKFILENAME as string               : rem Disk File Name...
DIM shared Slots as integer                             : rem How many directory slots are available.
DIM shared Filepointers as integer              : rem Either 1 or 2. How many bytes in the extent for each allocation.
DIM shared CrossWarning as integer              : rem Show Cross-linked Files if 1

REM Now for the fixed DSK variables.
REM DISK INFORMATION BLOCK.
DIM shared TITLE as string                              : rem 0-21 (1-34), 34 chars, Title of Disk File.        "MV - CPCEMU Disk-File\r\nDisk-Info\r\n"
DIM shared CREATOR as string                    : rem 22-2F (35-48), 14 chars, Creator Name
DIM shared TRACKS as Byte                               : rem 30 (49), 1 byte
DIM shared SIDES as Byte                                : rem 31 (50), 1 byte
DIM shared TRACKLEN as Integer                  : rem 32 (51), 2 bytes
DIM shared UNUSED as string                             : rem 34 (53), 204 bytes

DIM shared TOTALTRACKS as Integer               : rem Calculate by SIDES * TRACKS. Never ask for a track above this value.

rem TRACK INFORMATION BLOCK.
DIM shared TTITLE as string                             : rem 0-0c (1-13), 13 chars, Title of Tracks. "Track-Info\r\n"
DIM shared UNUSED2 as string                    : rem 0d-0f (14-16), 3 chars, Unused.
DIM shared TRACKNUM as Byte                             : rem 10 (17), 1 byte.
DIM shared SIDENUM as Byte                              : rem 11 (18), 1 byte.
DIM shared UNUSED3 as string                    : rem 12-13 (19), 2 bytes.
DIM shared Sectorsize as Byte                   : rem 14 (21), 1 byte.
DIM shared Numsectors as Byte                   : rem 15 (22), 1 byte.
DIM shared GAP3 as Byte                                 : rem 16 (23), 1 byte. Not sure what this is?
DIM shared Filler as Byte                               : rem 17 (24), 1 byte. Not sure what this is?
DIM shared SectorInfo as String                 : rem 18-FF (25-256), 232 bytes. Sector Information List. Note: Track Info Block takes up 256 bytes...
DIM shared SECTORS as String                    : rem 100 (256) - To end of Track = Sector Data. Expect up to 9216 = 9 sect/track * 512 * 2 ( for double density ). That's a lot.

DIM shared VIEWALLOC as integer                 : rem Which allocation to view.
DIM shared ALLOCS as integer                    : rem Maximum allocations on disk
DIM shared LOGICAL as integer                   : rem Logical Sectors... 0 to Max Sectors.
DIM shared MAXSECTORS as integer                : rem Maximum sectors.

DIM shared DISKIDENTIFIER as string             : rem 16 bytes of Disk Specification Identifier - This is AMSTRAD data to tell the computer what the disk is, not just the virtual DSK disk.
DIM shared AMType as integer                    : rem Byte 0, 0=standard DD DS ST +3 disk. 1=CPC DD SS ST system. 2= CPC DD SS ST Data Only Format, 3 = Standard DD DS DT disk (+3 compat )
DIM shared AMSides as integer                   : rem Byte 1, 0 = Single Sided, 1 = Double Sided Interleave, 2 = Side 1 then Side 2 in that order... Or maybe for more sides.
DIM shared AMTracks as integer                  : rem Byte 2, Number of tracks per side.
DIM shared AMSectors as integer                 : rem Byte 3, Number of Sectors per track.
DIM shared AMSectSize as integer                : rem Byte 4, Sector Size ( Log2 - 7 ) - So 2 = 2^9 = 512 bytes / sector. 1= 256 and 0 = 128. Should never be that small.
DIM shared AMActualSS as integer                : rem Calculated Sector size from Byte 4.
DIM shared AMReserved as integer                : rem Byte 5, Number of reserved tracks. Tracks that aren't DISK contents, or are System data.
DIM shared AMBlockSize as integer               : rem Byte 6, Block Size (Log2(Blocksize/128)) - Allocations - So 3 = 8 * 128 = 1024K. This is the ALLOCATION SIZE.
DIM shared AMActualBS as integer                : rem Calculated Block ( allocation ) size from Byte 6.
DIM shared AMDirectory as integer               : rem Byte 7, How many allocations are DIRECTORY allocations?
DIM shared AMGap as integer                             : rem Byte 8, Gap Length for read/write.
DIM shared AMGap2 as integer                    : rem Byte 9, Gap Length for Format.
DIM shared AMUnused as string                   : rem Bytes 10 to 14. Currently reserved.
DIM shared AMChecksum as integer                : rem Byte 15, Used for bootable disk only.
DIM shared SectorsPerAllocation	as integer				: rem How many sectors should we read for a single allocation??? Let's calculate it once.  We'll use it in loops later. Added with new TRANSLATE routine

DIM shared FATALLOCS as integer                 : Rem used in creating the FAT to identify which allocation is being addressed, whether 8 bit or 16 bit. Low byte first. 16 bit if ALLOCS > 255
DIM shared FREESPACE as integer                 : rem How much free space on disk...


dim shared FTRACKS as integer                   : rem When we format a disk. how many tracks to format.
dim shared FHEADS as integer                    : rem When we format a disk, how many heads to format.
dim shared FSECTORS as integer                  : rem When we format a disk, how many sectors per track. CHS ( = Tracks, Heads, Sectors ).
dim shared FSECTSIZE as integer                 : rem How big is a sector when we format a disk.
dim shared FTRACKNUM as integer                 : rem While formatting, which track are we on?
dim shared FSIDENUM as integer                  : rem While formatting, which side are we on?

DIM shared SERROR as integer                    : rem General error carry from routines and subroutines to detect failure of a subroutine and error out.
dim shared uzx(256) as string                   : rem Upper ZX characters, for showing ZX basic files.. Might have to deal with file headers first.


REM OLD VARIABLES BELOW HERE> TRY TO AVOID USING THEM SO I CAN COMBINE FILES LATER.
rem Microdrive below.


DIM shared INDATA as string                     : rem  Input buffer when reading files, console, etc.
DIM shared SECTOR(1280) as string               : rem  Array of all sectors, each sector is a strong, 543 bytes long.
DIM shared NEWCART(1280) as string              : rem  If we need to build a new cartridge file.

DIM shared HDFLAG as integer                    : rem 8 bit integers here. Always "1" for header.
DIM shared SECTORID as integer                  : rem Sector ID of currently selected sector.
DIM shared SECTORNUM as integer                 : rem SectorID=SectorNUM when a normal format... SectorNUM is the actual sector in SECTOR(SECTORNUM)
DIM shared NOTUSED as integer                   : rem Should be 00,00
DIM shared CNAME as string                      : rem CNAME is name of Cartridge.
DIM shared SUM15 as integer                     : rem Checksum of first fourteen bytes.

DIM shared RECFLG as integer                    : rem Bit 0,1 or 2. 04 for file, 06 for EOF. etc.
DIM shared RECNUM as integer                    : rem Record Number
DIM shared PAYLOAD as Integer                   : rem LSB in string, two bytes, but single 16 bit integer here, data payload.
DIM shared FNAME as string                      : rem Filename of the current sector or subsector.
DIM shared SUM30 as integer                     : rem Checksum of bytes 16 to 29.
DIM shared SUM543 as integer                    : rem Checksum of bytes 31 to 252

DIM shared BYTES as STRING                      : rem Read in bytes to be built into sector.
DIM shared FILEREAD as STRING                   : rem Stuff from file.
DIM shared FILESIZE as integer                  : rem How big is it?
DIM shared FILENAME as string                   : rem We will need a filename too.
DIM shared DESTINATION as integer               : rem Destination Address for code.
DIM shared LOWDEST as integer                   : rem Low byte of destination
DIM shared HIGHDEST as integer                  : rem High byte of destination

DIM shared CHECKSUM as integer                  : rem Just a working checksum scratchpad.
DIM shared TNAME as string                      : rem Buffer for names. CNAME and FNAME
DIM shared INTERLEAVE as integer

DIM shared a as integer                         : rem Because, dammit, I just want to do "for a = 1 to 100" or stuff like that sometimes...
DIM shared b as integer
DIM shared c as integer
DIM shared k as string                          : rem And a basic character

DIM shared LOWBYTE as integer                   : rem Scratchpad calculations
DIM shared HIGHBYTE as integer                  : rem scratchpad ...
DIM shared PREAMBLE as string                   : rem scratchpad...
DIM shared FL as integer                        : rem Filename number for lots.
rem DIM shared FS as string                             : rem STRING version of FL
rem FIX THIS WHEN PUTTING THE MICRODRIVE SOFTWARE BACK IN.


function Argument (TXT as string, arg as integer) as string
dim s as integer        :rem Position in string.
dim start as integer    :rem start position.
dim spos as integer      :rem what argument are we at?
dim result as string      :rem RESULT.

        s=1
        spos=0

        result=""

        while mid(TXT,s,1)=" "      : rem Kill any whitespace first.
                s=s+1
        wend

        if len(TXT)=0 then goto SCANEND
SCAN1:
                if mid(TXT,s,1)<>chr(34) and mid(TXT,s,1)>" " then
                while mid(TXT,s,1)>" " and s<=len(TXT)  : rem Stop if ANY control characters come through too unless quoted.
                        result=result+mid(TXT,s,1)
                        s=s+1
                wend
        endif

        if mid(TXT,s,1)=chr$(34) then
                s=s+1
                while mid(TXT,s,1)<>chr(34) and s<=len(TXT)
                result=result+mid(TXT,s,1)
                s=s+1
                wend
                s=s+1                   : rem account for final quote space.
        endif

        while mid(TXT,s,1)=" "
                s=s+1
        wend

        spos=spos+1
        if spos <arg and s<=len(TXT) then result="": goto SCAN1


SCANEND:
        if arg>spos then result=""
        return result
end function









function Argument2(ByRef Indata as string, Argnum as Integer) as string

dim OutData as string   : rem The string we're returning.
dim s as integer        : rem String Position.
dim start as integer    : rem Start of current argument.
dim f as integer        : rem f = current argument number.
dim inquotes as integer : rem if not 0, then we're "In Quotes" and should be one argument.

        OutData = ""
        inquotes=0
        s=1: f=0

        ArgLoop1:
        print "B";asc(mid$(Indata,s,1)),

                if mid$(InData,s,1)=chr(34) then
                        inquotes=1
                        print "WE GOT ONE"
                        s=s+1   : rem Ignore the first quote.
                endif
                if mid$(InData,s,1)=" " and inquotes=0 then s=s+1
                if s < len(Indata) and mid$(InData,s,1)=" " and inquotes=0 then goto Argloop1:

        f=f+1
        start=s
        print "Inquotes:";inquotes

        Argloop2:
        print "A";asc(mid$(Indata,s,1)),
                if mid$(InData,s,1)<>chr$(34) and inquotes=1 then s=s+1 : goto Argloop2:
                if mid$(Indata,s,1)<>" " and inquotes <> 1 then s=s+1
                if s < len(InData) and mid$(InData,s,1)<>" " then goto Argloop2:

        if f<Argnum then inquotes=0: goto Argloop1:

        if s>start then OutData=mid(InData,start,s-start)

        if right(OutData,1)=chr$(34) then OutData=left(outdata,len(outdata)-1)  : rem Correct for final quote if quotes.

        Argfound:
        return OutData
end function


Function LoadFile(ByRef filename As String) As String                                   : rem Load in a file from the hard disk. Used to load in DISK DSK files.

    Dim h As Integer
    Dim txt As String

    h = FreeFile

    If Open( filename For Binary Access Read As #h ) <> 0 Then Return ""

    If LOF(h) > 0 Then

        txt = String(LOF(h), 0)
        If Get( #h, ,txt ) <> 0 Then txt = ""

    endIf

    Close #h
        DefaultDsk = filename                                                                                           : rem Note the last file opened, as sometimes we need to save it.

    Return txt

End Function

Function SaveFile(ByRef filename As String)     as String                                                       : rem Load in a file from the hard disk. Used to load in DISK DSK files.

    Dim h As Integer
    Dim txt As String

    h = FreeFile

    open filename for output as #h

        print #h, DISK;

    Close #h

    Return txt                                                                                                                  : rem Need to work out what to return here. If anything.

End Function


Function FileExists(ByRef filename As String) as Integer                                                       : rem
rem Test if a file exists and return the error code.

    Dim h As Integer
    Dim result as integer

    h = FreeFile

    result=Open( filename For Binary Access Read As #h )

    Close #h

    Return result

End Function


Function SaveAsFile(ByRef File as string, ByRef filename As String)     as String                                                       : rem Load in a file from the hard disk. Used to load in DISK DSK files.

    Dim h As Integer
rem    Dim txt As String

    h = FreeFile

    open filename for output as #h

        print #h, File;

    Close #h

    Return File                                                                                                                 : rem Need to work out what to return here. If anything.

End Function

Function Insert(ByRef target as String, ByRef contents as String, Byref location as integer) As String                  : rem Insert a string into a string. For modifying a DSK file.

        dim li as Integer : rem Length of inserted string.
        dim lt as Integer : rem Length of targetted string.

        target=left$(target,location-1)+contents+right$(target,len(target)-len(contents)-location+1)

        Return target
End function

Function GetTrack (Byref image as string, Byref tnum as integer) As String

        dim txt as string
        dim offset as integer

        rem Used to get the Disk Information Block variables set here. Now I just set the TIB data once the track is loaded.

        txt=mid$(image,(tracklen*tnum)+257,tracklen)
        TTITLE=mid$(txt,1,13)
        UNUSED2=mid$(txt,14,3)
        TRACKNUM=ASC(MID$(txt,17,1))
        SIDENUM=ASC(MID$(txt,18,1))
        UNUSED3=MID$(txt,19,2)
        Sectorsize=ASC(MID$(txt,21,1))
        Numsectors=ASC(MID$(txt,22,1))
        GAP3=ASC(MID$(txt,23,1))
        Filler=ASC(MID$(txt,24,1))
        Sectorinfo=MID$(txt,25,232)
        SECTORS=MID$(txt,257,(Sectorsize*Numsectors))

        Return txt

End function


Function GetLogicalTrack (Byref image as string, Byref tnum as integer) As String

        dim txt as string
        dim offset as integer

        rem Track 0 is side 0. Track 1 may be Track 0 Side 1, or may be Track 1 side 0.  Depends on whether double sided.

        txt=mid$(image,(AMSectors*AMActualSS*tnum)+257+(tnum*256),(AMSectors*AMActualSS)+256)

        Return txt

End function





Function Translate (Byref sector as integer, Byref track as integer) as integer
rem sector is the sector we want to know ( where is the physical location of logical sector ). Number from 1 to 28 ( 32 is the limit for some reason ). 
rem track is the track that the logical sector is on. 
rem We access the TIB and see what the interleave is.. NOTE THIS REPLACES DETECTINTERLEAVE FUNCTION... ( below ). 

dim newsector as integer	: rem What is the new sector?
dim physical as integer		: rem What is the physical sector we are looking at? ( this would be the format sector marker on the disk )
dim BLOCK as string			: rem We use it elsewhere, but let's not contaminate other BLOCKs..  Block of Track Info.
dim SPOINT as integer			: rem Sector Location Pointer. Where in the block is the logical sector indicator...
dim tlist(32) as integer		: rem List of translated sectors, because we have to search for them... AMSectors is the max number of sectors we search through.
dim localloop as integer		: rem Local loop variable.

rem Read the Track Information Block that maps sector order to physical order ( logical sectors can be out of order, even on a track by track basis ).
        BLOCK=getlogicaltrack(DISK,track)

	for localloop = 1 to AMSectors
		SPOINT=16+(localloop*8)+3	: rem Location of Interleave Information.
		physical=asc(mid(BLOCK,SPOINT,1)) 	: rem What sector resides at this location?  (logical sector number is in localloop, physical is located at this place in the TIB )
		tlist(physical)=localloop			: rem Record what physical sector ( eg, in file ) exists for this logical sector.
rem		print physical,tlist(physical),localloop
	next localloop					: rem and we populate the table.

	if tlist(sector) <> 0 then
		newsector=tlist(sector)
	else
		newsector=sector	: rem if there's no translation, then go with the logical=physical order. (no interleave ).
	endif

	return newsector
End function






Function GetSect (Byref image as string, Byref logicalsector as integer) as string
        rem - Reads a sector based on the logical sector number, eg, 1 is the boot sector.

        dim xtrack as integer                   : rem Let's calculate the track, as well as the subsector.
        dim xsector as integer                  : rem Once we know which sectors are needed, we need to recalculate for which sector on that track is needed before translating for interleave.
        dim sectorindex as integer              : rem Locate where the sector will go as an address
        dim result as string                    : rem What result will we return?

        result=""                                               : rem clear result.

        if logicalsector<1 then goto getsectorerror                                             : rem No Sector 0.
                                                                                                                                        : rem Later test for reading past last sector too.

        xtrack=int((logicalsector-1)/AMSectors)                                                 : Rem Convert to Track and Sector so we can calculate correct offset into DISK FILE.
        xsector=( (logicalsector-1) mod AMSectors ) +1
		
rem 		Print "Debug(function getsect)-  Track:";xtrack;"   Sector:";xsector

rem                xsector=SINTERLEAVE(xsector)                                                            : Rem Translate sector to address interleave if required.
               xsector=translate(xsector,xtrack)                                                               : Rem Translate sector to address interleave if required. Use new routines. 			
	
                sectorindex=256                                                                         : rem DISK INFORMATION BLOCK size

                sectorindex=sectorindex+(xtrack*AMActualSS*AMSectors)+(xtrack*256)+256                  : rem Add in a Track Information Block for ALL current tracks previous ( Tracks start at 0 ) and for the current track PLUS the previous track lengths.

                sectorindex=sectorindex+((xsector-1) * AMActualSS)                                                              : rem Now add in the current sectors... Sector 1 is at ZERO offset now, so remove 1 from sector count.

                sectorindex=sectorindex+1                                                                                                               : rem It's in an array. Offset 0 of the file is at position 1 of the array. Add one.

        result=result + mid(image,sectorindex,AMActualSS)

getsectorerror:
        rem     if there is any error, then just return here.

        Return result
End function

Function PutSect (Byref image as string, Byref text as string, Byref logicalsector as integer) as string
        rem - Writes a sector based on the logical sector number, eg, 1 is the boot sector.
        rem Image = Disk Image
        rem text = text to write. Should be same size as sector, but might be smaller. Truncate if longer.
        rem logicalsector = sector to write it to.

        dim xtrack as integer                   : rem Let's calculate the track, as well as the subsector.
        dim xsector as integer                  : rem Once we know which sectors are needed, we need to recalculate for which sector on that track is needed before translating for interleave.
        dim sectorindex as integer              : rem Locate where the sector will go as an address
        dim wsector as string                   : rem The sector data to write.
        dim result as string                    : rem What result will we return?

        result=""                                               : rem clear result.
        wsector=left(text+string(AMActualSS,00),AMActualSS)     : rem Pad out any remaining data and truncate.

        if logicalsector<1 then goto getsectorerror                                             : rem No Sector 0.
                                                                                                                                        : rem Later test for reading past last sector too.

        xtrack=int((logicalsector-1)/AMSectors)                                                 : Rem Convert to Track and Sector so we can calculate correct offset into DISK FILE.
        xsector=( (logicalsector-1) mod AMSectors ) +1

rem                xsector=SINTERLEAVE(xsector)                                                            : Rem Translate sector to address interleave if required.
                xsector=translate(xsector,xtrack)                                                            : Rem Translate sector to address interleave if required.
				
				
                sectorindex=256                                                                                                                                 : 
				rem DISK INFORMATION BLOCK size
                sectorindex=sectorindex+(xtrack*AMActualSS*AMSectors)+(xtrack*256)+256                  : rem Add in a Track Information Block for ALL current tracks previous ( Tracks start at 0 ) and for the current track PLUS the previous track lengths.
                sectorindex=sectorindex+((xsector-1) * AMActualSS)                                                              : rem Now add in the current sectors... Sector 1 is at ZERO offset now, so remove 1 from sector count.
                sectorindex=sectorindex+1                                                                                                               : rem It's in an array. Offset 0 of the file is at position 1 of the array. Add one.

        result=insert(image, wsector, sectorindex)                                                                                      : rem Now we put the sector data supplied into the correct sector.

getsectorerror:
        rem     if there is any error, then just return here.

        Return result
End function






Function GetAllocation (Byref image as string, Byref allocation as integer) as string
        rem - Reads an allocation, opposite of put allocation.
		rem - Updated so it reads the sectors automatically with getsector. 

        dim lsector as integer               	: rem Which logical sector?
		dim	sectornum as integer					: rem for multiple sectors - eg, lector might be 0, but sectornum = 1... Sector num from 1 to sectorsperallocation
        dim result as string                    : rem What result will we return?

        result=""                                               : rem clear result.

		lsector=allocation*SectorsPerAllocation					: rem Find index of sectors ( will be 0 for first allocation 0 ). We will add 1 to SectorsPerAllocation to is later in our loop since sectors start with 1.
		lsector=lsector+(AMSectors*AMReserved)					: rem Make allowance for the number of reserved sectors in the reserved track. 

		for sectornum=1 to SectorsPerAllocation					: rem How many sectors in the allocation? Minimum 1.	
			result=result+GetSect(image,sectornum+lsector)			: rem Pick up each sector in the allocation. Wherever it is. 
		next	sectornum

        Return result
End function


Function PutAllocation (Byref image as string, Byref filedata as string, Byref allocation as integer) as string
        rem - Writes an allocation, opposide of get allocation.
		rem - Updated so it reads the sectors automatically with putsect. 

        dim lsector as integer               	: rem Which logical sector?
		dim	sectornum as integer					: rem for multiple sectors - eg, lector might be 0, but sectornum = 1... Sector num from 1 to sectorsperallocation
        dim result as string                    : rem What result will we return?
		dim chunk as string						: rem we write a sector sized chunk of the allocation at a time.

        result=""                                               : rem clear result.

		lsector=allocation*SectorsPerAllocation					: rem Find index of sectors ( will be 0 for first allocation 0 ). We will add 1 to SectorsPerAllocation to is later in our loop since sectors start with 1.
		lsector=lsector+(AMSectors*AMReserved)					: rem Make allowance for the number of reserved sectors in the reserved track. 

		for sectornum=1 to SectorsPerAllocation					: rem How many sectors in the allocation? Minimum 1. 
			chunk=mid(filedata,((Sectornum-1)*AMActualSS)+1,AMActualSS)	: Rem - Get a sector sized chunk of data... ( sector 1 = Chunk 0 )
			result=putsect(image,chunk,sectornum+lsector)
		next	sectornum

        Return result 
End function





Function tPutAllocation (Byref image as string, Byref filedata as string, Byref allocation as integer) as string
        rem - Writes an allocation, opposide of get allocation.
        dim subsector as string                 : rem Information to store in a single sector... Store in subsector.
        dim xtrack as integer                   : rem Let's calculate the track, as well as the subsector.
        dim xsector as integer                  : rem Once we know which sectors are needed, we need to recalculate for which sector on that track is needed before translating for interleave.
        dim chunk as integer                    : rem Which chunk of filedata should be in subsector?
        dim putsectors as integer               : rem Which logical sector?
        dim sectorindex as integer              : rem Locate where the sector will go as an address
        dim result as string                    : rem What result will we return?
        result=image                                    : rem Let's make the result the same as the image for now.
        chunk=0
        for putsectors=(allocation*(AMActualBS/AMActualSS))+1 to ( (allocation*(AMActualBS/AMActualSS))+(AMActualBS/AMActualSS))        : rem Calculate the logical sectors in the allocation.
                subsector=mid(filedata,(chunk*AMActualSS)+1,AMActualSS)         : rem What data do we want to write from the total?


                sectorindex=256+ ((AMReserved*AMSectors*AMActualSS)+256) +  ((int((putsectors+AMSectors-1)/AMSectors))*256) + ((putsectors-1)*AMActualSS) + 1   : rem 256 = Disk Information Block in DSK file.
                                                                                                                                                                                                                                                                        rem   Reserved tracks = 256+n Sectors, so calculate those.
                                                                                                                                                                                                                                                                        rem Then actual logical sectors *
                                                                                                                                                                                                                                                                        rem Then the number of Track Information Blocks we will pass ( 256 for each ).
                                                                                                                                                                                                                                                                        rem +1 finishes by allowing for the array offset( position 0=1)

                rem I need to adjust this here, so I can cope with Interleave... Either that, or I have to deinterleave the file on loading, but that's even harder.
                rem So let's calculate which actual sector we want.


rem             print
rem             print "Laying down a sector..."
rem     print "Part of allocation:";allocation
rem             Print "WAS Sectorindex says:";sectorindex;" and Putsectors is:";putsectors
                xtrack=int((putsectors-1)/AMSectors)                                                    : Rem WE DON'T ADD RESERVED TRACKS HERE, because it's the DATA TRACK = ie, Tracks - Reserved tracks.
                xsector=( (putsectors-1) mod AMSectors ) +1

rem print "Track:";xtrack;" Sector:";xsector
rem                xsector=SINTERLEAVE(xsector)                                                            : Rem Translate sector to address interleave if required.
                xsector=translate(xsector,xtrack)                                                            : Rem Translate sector to address interleave if required.
rem     print "Translated Sector:";xsector

                sectorindex=256                                                                                                                                 : rem DISK INFORMATION BLOCK size.
rem print
rem print "AMreserved:";AMReserved;"    AMActualSS:";AMActualSS;"    AMSectors:";AMSectors
rem Print "NEW Sectorindex says:";sectorindex;"    (hex):";hex(sectorindex)
                sectorindex=sectorindex+(Amreserved*AMActualSS*AMSectors)+(AMreserved*256)              : rem All reserved sectors + All Track Information Blocks in reserved area.
rem Print "NEW Sectorindex says:";sectorindex;"    (hex):";hex(sectorindex)
                sectorindex=sectorindex+(xtrack*AMActualSS*AMSectors)+(xtrack*256)+256                  : rem Add in a Track Information Block for ALL current tracks previous ( Tracks start at 0 ) and for the current track PLUS the previous track lengths.
rem Print "NEW Sectorindex says:";sectorindex;"    (hex):";hex(sectorindex)
                sectorindex=sectorindex+((xsector-1) * AMActualSS)                                                              : rem Now add in the current sectors... Sector 1 is at ZERO offset now, so remove 1 from sector count.
rem Print "NEW Sectorindex says:";sectorindex;"    (hex):";hex(sectorindex)
                sectorindex=sectorindex+1                                                                                                               : rem It's in an array. Offset 0 of the file is at position 1 of the array. Add one.

rem Print "NEW Sectorindex says:";sectorindex;"    (hex):";hex(sectorindex)

result=Insert(result,subsector,sectorindex)     : rem Write the data to the image, into the relevant logical sector.
rem Take the above line out to prevent writing.

                chunk=chunk+1
        next putsectors
        Return result
End function

Function Printhex (Byref image as string, Byref numhex as integer) as integer

        rem Image is the string you want to dump as HEX.
        rem numhex is the number of characters you want to display ( won't be less than 32 displayed... Might adjust that later. )

        dim a as integer
        dim b as integer
        dim c as integer

        print "         !           !           !           !           !           !           !           !           "

        for a=0 to int(((numhex-1)/32))

                print hex$(A*32,4);"  -  ";                             : rem Print relative location in Hex at start of line.

                for b=1 to 32
                        print hex$(asc(mid$(image,(a*32)+b,1)),2);" ";  : rem Print out the hex of the bytes.
                next b

                print "  -  ";

                for b=1 to 32                                                                   : rem Now print out the ASCII characters.
                        c=asc(mid$(image,(a*32)+b,1))
                        if c<32 then c=asc("+")
                        if c>127 then c=asc("+")
                        print chr$(c);
                next b

                print

        next a

        Return len (image)

End function

Function Literalhex (Byref image as string, Byref startpos as integer, Byref endpos as integer) as integer

        rem SAME as printhex, however, prints LITERAL offset, not relative. First character is position 0...
        rem Image is the string you want to dump as HEX.
        rem numhex is the number of characters you want to display ( won't be less than 32 displayed... Might adjust that later. )

        dim a as integer
        dim b as integer
        dim c as integer

        dim sline as integer    : rem Starting Line, because we may not know which line to start at. Start at 32 byte boundary.
        dim eline as integer    : rem Because likewise, we don't know where it will end.
        dim last as integer

        sline=int((Startpos)/32)
        print "Starting:";sline
        eline=int((endpos)/32)
        print "Ending:";eline


        print "           !           !           !           !           !           !           !           !           "

        for a=sline to eline

                        print hex$(A*32,6);"  -  ";                             : rem Print relative location in Hex at start of line.

                for b=1 to 32
                        if (a*32)+b > startpos-1 and (A*32)+b < endpos+1 then
                                print hex$(asc(mid$(image,(a*32)+b,1)),2);" ";  : rem Print out the hex of the bytes.
                        else
                                print "-- ";    : rem Otherwise just print spaces or blanks.
                        endif
                next b

                print "  -  ";

                for b=1 to 32                                                                   : rem Now print out the ASCII characters.
                        c=asc(mid$(image,(a*32)+b,1))
                        if c<32 then c=asc("+")
                        print chr$(c);
                next b

                print

                if (a*32)+b > endpos then a=eline

        next a

        OverflowHex:

        Return len (image)

End function

rem ################### File Handling Functions ########################


Function Casefix (Byref fname as string) as string
        dim cpos as integer
        dim result as string
        result=""
        for cpos = 1 to len(fname)
        if mid(fname,cpos,1) >= "a" and mid(fname,cpos,1) <="z" then
                result=result+chr((asc(mid(fname,cpos,1))-32) mod 128)
                else
                result=result+chr((asc(mid(fname,cpos,1))) mod 128)     : rem MOD 128 removes any hidden bits ( bit 7 )
                endif

        next cpos
        return result
End function

Function Matchfilename (Byref fname as string, dirfile as string) as integer
        rem Get the filename specified including wildcards, and validate against the directoryname stored in FAT and FILES.
        rem Call with both names.  dirfile should just be 11 characters and space-padded.

        dim posi as integer             : rem position in the string search.
        dim match as integer    : rem Do we have a match?
        dim exname as string    : rem expanded file name.
        dim decp as integer             : rem location of decimal point.
        dim ofile as string             : rem Where are we going to stick the output file as we build it?

        decp=instr(fname,".")                           : rem Clean up the incoming name by spacing correctly and addressing the dot point.
        if decp=0 then
                        exname=fname+"           "
                        exname=left$(exname,11)
        else
                        exname=mid$(fname,1,decp-1)+"        "
                        exname=left$(exname,8)
                        exname=exname+mid$(fname,decp+1,3)+"   "
                        exname=left$(exname,11)
        endif
        exname=casefix(exname)
        dirfile=casefix(dirfile)         : rem Make BOTH upper case. Leave other characters intact. File system is case insensitive.

        match=1

                for posi=1 to 11

                        if mid(exname,posi,1)=chr(asc(mid(dirfile,posi,1)) mod 128) then goto scanon2 : rem Same Character ( case insensitive )
                        if mid(exname,posi,1)="?" then goto scanon2 : rem Wildcard ?
                        if mid(exname,posi,1)="*" and posi<9 and decp<>0 then posi=8 : goto scanon2 : rem * wildcard in name section.
                        if mid(exname,posi,1)="*" and posi<9 and decp=0 then posi=11 : goto scanon2 : rem * wildcard in name section, and no decimal point. Match all suffixes.
                        if mid(exname,posi,1)="*" and posi>8 then posi=11 : goto scanon2 : rem * wildcard in type section.
                        match=0 : rem if it gets this far, it's not a match.
        scanon2:
                next posi

                if debug > 0 and match=0 then print "No Match [";exname;"] and [";dirfile;"]"

Matchfilenameend:
        return match

End function




Function Matchfile (Byref fname as string, Byref position as integer) as string

        rem Match any file. Sanitise for upper and lower case characters, so is not case dependent.
        rem if Position is not specified or is 0, it just returns the first match. Otherwise positions can be skipped for multiple matches. Will make a nummatches routine to identify how many files match.

        dim match as integer    : rem Do we have a match?
        dim exname as string    : rem expanded file name.
        dim dirfile as string   : rem directory file name, so I don't have to keep using mid$
        dim decp as integer             : rem location of decimal point.
        dim ofile as string             : rem Where are we going to stick the output file as we build it?

        decp=instr(fname,".")                           : rem Clean up the incoming name by spacing correctly and addressing the dot point.
        if decp=0 then
                        exname=fname+"           "
                        exname=left$(exname,11)
        else
                        exname=mid$(fname,1,decp-1)+"        "
                        exname=left$(exname,8)
                        exname=exname+mid$(fname,decp+1,3)+"   "
                        exname=left$(exname,11)
        endif
        exname=casefix(exname)

        rem print "Fixed:";exname


        for a=1 to numfiles
                match=1
                dirfile=mid$(files(a),2,11)
rem             print "Checking ";exname;" against ";dirfile
                for b=1 to 11
rem debug               print b;":";
rem debug print hex(asc(mid(exname,b,1)),2);":";hex(asc(mid(dirfile,b,2)),2)
                        if mid(exname,b,1)=mid(dirfile,b,1) then goto scanon : rem Same Character ( case insensitive )
                        if mid(exname,b,1)="?" then goto scanon : rem Wildcard ?
                        if mid(exname,b,1)="*" and b<9 then b=8 : goto scanon : rem * wildcard in name section.
                        if mid(exname,b,1)="*" and b>8 then b=11 : goto scanon : rem * wildcard in type section.
                        match=0 : rem if it gets this far, it's not a match.
        scanon:
                next b
rem debug               print
                if match=1 then print "dirfile matched - ";dirfile
rem debug               print
rem debug               print
        next a


return ""
End function

Function PutDirEntry (Byref ThisExtent as string) as string

dim acount as integer
dim bcount as integer
dim DIRALLOCATION as string                                                                     : rem temporarily hold the diretories allocation while we update the directory.

                for acount=0 to AMDirectory-1
                        DIRALLOCATION=GetAllocation(DISK,acount)
                        for bcount=0 to AMActualBS-32 step 32
rem debug print "Scanning:";mid(DIRALLOCATION,bcount+2,11)
                        if asc(mid(DIRALLOCATION,bcount+1,1))=229 then
                                DIRALLOCATION=insert (DIRALLOCATION,ThisExtent,bcount+1): goto PutDirUpdated            : rem Insert the new extent record.
                        endif
                        next bcount
                next acount                                                                                     : rem Next Allocation.

PutDirUpdated:
                DISK=putallocation(DISK,DIRALLOCATION,acount)                   : rem Update disk image...
        return DIRALLOCATION
End function


Function CleanFilename (Byref fname as string) as string
        dim exname as string    : rem Handling variable.ddd
        dim decp as integer             : rem Place of Decimal Point in filename
        dim clean as integer    : rem Clean counter to remove characters that are not acceptable.
        exname="           " : rem 11 blank characters. Let's clean up and format the file name with padding.
        decp=instr(fname,".")
        if decp=0 then
                        exname=fname+exname
                        exname=left$(exname,11)
        else
                        exname=mid$(fname,1,decp-1)+"        "
                        exname=left$(exname,8)
                        exname=exname+mid$(fname,decp+1,3)+"   "
                        exname=left$(exname,11)
        endif

        for clean = 1 to len(exname)                                                                                                                                                    : rem remove any reserved characters.
                if mid(exname,clean,1)="*" or mid(exname,clean,1)="?" then exname=insert(exname,"-",1)
        next clean

        rem - I might have to make a clean up routine some day to mask bit7 of the filenames that are stored.
        return exname
End function

Function SetNextAllocation ( Byref filenum as integer ) as integer                                                                      : rem Return the next allocation number from the FAT.
        dim allsearch as integer

        for allsearch=AMDirectory to allocs
                if fat (allsearch+1)=0 then fat (allsearch+1)= filenum  : return allsearch                                                                      : rem Allocate this fat location to filenum. Allocation is one less than in FAT due to array starts at 1 while allocation starts at 0
        next allsearch

        return -1       : rem Failed to provide an allocation ( Shouldn't happen ).

End function





Function tDetectInterleave (Byref image as string) as Integer

dim outloop as integer          : rem another local loop
dim localloop as integer
dim testsort(32) as integer     : rem quick check of interleave.
dim detect as integer
dim test as integer                     : rem quick variable for a test.
detect=0
test=0

rem: Assume sectors MUST BE less than 32. Because we can't store more than about 28.

if AMSectors < 5 or AMSectors > 31 then goto Detecterror

	for localloop=1 to 32
		testsort(localloop)=0
	next localloop


	for	 outloop=1 to AMSectors
		for localloop=1 to AMSectors
			if      outloop=asc(mid(IMAGE,((localloop-1)*8)+&H011B,1)) mod 32 then
				SINTERLEAVE(outloop)=localloop
			endif
			testsort(SINTERLEAVE(localloop))=1
		next localloop
	next outloop

	rem For localloop=1 to AMSectors
	rem     print SINTERLEAVE(localloop);" ";
	rem next localloop
	rem print


	rem Now test if they are all present.
	for localloop=1 to AMSectors
		if SINTERLEAVE(localloop)<>1 then test=1
	next localloop

	detect=test     : rem If test =1 then the interleave function has been determined.

Detecterror:
	rem Come in here if there's a fatal error.

	if AMSectors < 5 or AMsectors > 31 then detect=0        : rem Error condition. DOn't assume interleaving because nothing got checked.

	if detect = 0 then                                                                      : rem if any invalid interleave detected then kill off all of the Interleave readings and make it linear
		for localloop=1 to 255
			SINTERLEAVE(localloop)=localloop
        next localloop
		rem     print "Removing Interleave"
	endif

	For localloop=1 to AMSectors
	rem     print SINTERLEAVE(localloop);" ";
	next localloop
	rem print

	return detect
end function


Function Default (Byref image as string) as integer
rem Select from several default directory options, unless a specifier has been located.
rem We will create a default signature at 000C0H at the start of the disk image and
rem data from 000E0H to define the disk type for a manual override.
rem Format is as follows;
rem 000E0 - "OVERRIDE"
rem 000E8 - Type
rem 000E9 - Sides
rem 000EA - Tracks
rem 000EB - Sectors
rem 000EC - Sector Size
rem 000ED - Reserved Tracks
rem 000EE - Blocksize
rem 000EF - Directory Allocations

rem If OVERRIDE is found, the set as per the above, and ignore autocorrection.
rem
rem Otherwise look for other clues as to what the disk format should be.
rem

dim Response as integer
rem Response is what we return. It will always be at least a 1 here.
Response=1

rem Base Default Settings. We need something here... We will modify this as we go.
                AMType=0
                AMSides=0
                AMTracks=40
                AMSectors=9
                AMSectSize=2

                AMReserved=1
                AMBlockSize=3

                AMDirectory=2
                AMGap=42
                AMGap2=82
                rem 11 to 15 are reserved.. All should be zero. We don't set any variables here.

                TRACKLEN=4608+256
                TRACKS=40
                Sides=0

rem TEST for AMSTRAD DISKS. C1= No reserved directories, 41 = two reserved directories.
rem Response 2. Amstrad default disk. As above, but set response differently.
        if mid(image,&H011B,1)=chr(&H041) then                                  : rem Location is 11A, but we need to add 1 since we hold it in a character array which starts from 1.
                response=2
                AMReserved=2                                                                            : rem Two tracks for this format.
                Print "Amstrad System Disk Image detected"
        endif

        if mid(image,&H011B,1)=chr(&H0C1) then
                response=3                              : rem No reserved tracks.
                Print "Amstrad Data Disk Image detected"
                AMReserved=0
        endif




rem Look for OVERRIDE SIGNATURE HERE. We will override specific or all values now if we find it.

rem Calculated values we rely upon.
                AMActualSS=2^(AMSectSize+7)
                AMActualBS=(2^AMBlockSize)*128
				SectorsPerAllocation=AMActualBS/AMActualSS


return response
End function

Function INIT ( Byref image as string ) as integer
rem Function version of INIT - Can initialize a specific DISK image, other than DISK.

        TITLE=mid$(image,1,34)
        CREATOR=mid$(image,35,14)
        TRACKS=ASC(mid$(image,49,1))
        SIDES=ASC(mid$(image,50,1))
        TRACKLEN=ASC(mid$(image,51,1))
        TRACKLEN=TRACKLEN+(256*ASC(mid$(image,52,1)))
        UNUSED=mid$(image,53,204)
        TOTALTRACKS=SIDES*TRACKS

        DISKTRACK=getlogicaltrack(image,1)      : rem Also get Track Information Block of the current track.

        dim localloop as integer


        DISKIDENTIFIER=mid$(image,513,16)

        rem  There is a default format for Amstrad disks too. All bytes are E5.
        amdefault=1
        for localloop=1 to 16
        if asc(mid(DISKIDENTIFIER,localloop,1))<>229 then amdefault=0
        next localloop


                XDPB=mid(DISKIDENTIFIER,1,16)

                REM Initialise Amstrad Data from DISK Image.

                AMType=asc(mid$(DISKIDENTIFIER,1,1))
                AMSides=asc(mid$(DISKIDENTIFIER,2,1)) mod 8
                AMTracks=asc(mid$(DISKIDENTIFIER,3,1))
                AMSectors=asc(mid$(DISKIDENTIFIER,4,1))
                AMSectSize=asc(mid$(DISKIDENTIFIER,5,1))
                AMActualSS=2^(AMSectSize+7)
                AMReserved=asc(mid$(DISKIDENTIFIER,6,1))
                AMBlockSize=asc(mid$(DISKIDENTIFIER,7,1))
                AMActualBS=(2^AMBlockSize)*128
                AMDirectory=asc(mid$(DISKIDENTIFIER,8,1))
                AMGap=asc(mid$(DISKIDENTIFIER,9,1))
                AMGap2=asc(mid$(DISKIDENTIFIER,10,1))
                rem 11 to 15 are reserved.. All should be zero.
				
				SectorsPerAllocation=AMActualBS/AMActualSS

        if AMType > 64 and AMType < 91 then amdefault=default(image)
        if AMType > 16 then amdefault=default(image)                                                                            : rem Yes, this makes the previous line worthless.

        rem Sanitize AM values in case it's NOT an amstrad disk. Bad values for these two can cause crashes.




        if AMActualSS < 256 or AMActualSS > 16384 then amdefault=default(image)
        if AMActualBS > 16384 then amdefault=default(image)


allocs=int(   ( AMsectors*((AMtracks*(AMsides+1))-AMReserved) )  /  (AMActualBS/AMActualSS)   )                 : rem Track 0, Other side, is a DATA track. Reserved tracks are SINGLE SIDED...

        if allocs>255 or AMActualBS >1024 then filepointers=2 else filepointers=1

        for a=1 to 256: Files(a)=string(256,0) : next a : rem MAKE file space for filenames... Preload strings.

rem     DIRECTORY       : rem Call Directory Subroutine, Build Directory.

rem DetectInterleave(DISK) : rem We eliminated this function. Replaced with translate which checks the TIB each time it accesses a sector. 

return 0

end function

Function StripBit7 ( Byref text as string ) as string

dim response as string                  : rem We return a string.
dim localloop as integer                : rem there is a loop involved.
dim onechar as integer                  : rem Lets hold the value of 1 character here.

response=""


for localloop = 1 to len(text)
        onechar=asc(mid(text,localloop,1)) mod 128
        if onechar<32 then onechar=32
        response=response+chr(onechar)
next localloop

return response

end function


Function BuildFAT ( fnum as integer ) as integer

dim record as integer

rem Scans the allocation tables following a filename ( 16 bytes, either 16 or 8 entries, depending on number of allocations )
rem Then marks in a table which file is using which allocation.
rem Can be later used in DEFRAG and other operations.
rem Right now it's used to build the FAT matrix and identify which allocations are in use across the entire disk.

        for record=0 to 15
                fatallocs=asc(mid(DISKALLOCATION,dircount+16+record,1))
                if allocs>255 then record=record+1: fatallocs=fatallocs+(asc(mid(DISKALLOCATION,dircount+16+record,1))*256) : rem Add in upper pointer to allocation. ( records are two bytes in this case )
                if fatallocs > allocs then print "Warning: Bad Allocations in Directory - Allocation:";fatallocs,"File:";mid(diskallocation,dircount+1,11)
                if fatallocs <> 0 and fatallocs < allocs then
                        if fatallocs <> 0 and fat(fatallocs+1) <> 0 then
                                if CrossWarning=1 then print "Warning: Cross-linked Allocations in file:";StripBit7(mid(DISKALLOCATION,dircount+1,11))
                        else
                                fat(fatallocs+1)=fnum   : rem record which file had the allocation in the allocation.
                        endif
                else
                        record=15
                endif
        next record

return 0
end function



Function BuildDirectory (image as string ) as integer

rem Builds the FAT, compiles the directory list (files variable) and identifies any directory issues such as cross-linked files.

        dim S1 as integer               : rem Reserved 2.2...  Otherwise it's a sub-record indicator of the final record. Low bytes, so to speak.
        dim RC as integer               : rem Record Counter

        Extent=0 : rem Extent of File ( which extent it is ) Calculate later
        PriorExtent=0 : rem Previous Extent of stored file saved here.
        Filelength=0 : rem File Length under Cp/M is zero. Calculate later.
        PATH="" : rem The path other than the root. \imagename.img\directory\filename.txt   Can copy between images.  Should be stateless.
        fileend=0 : rem Number of bytes in the final allocation.
        numfiles=0 : rem Number of files in the directory.
        root=1 : rem First cluster is directory.
rem        for a=1 to 360: fat(a)=0 : next a : rem Clear FAT tables. We will rebuild them.
        for a=1 to allocs: fat(a)=0 : next a : rem Clear FAT tables. We will rebuild them.
        for a=1 to 256: Files(a)=string(2048,0) : next a : rem MAKE file space for filenames... Preload strings. Im allowing 2048 bytes per string = 64 extents. = 64 * 16K = 1Mb per file. Will have to fix this if I get a hard disk and allow subdirectories.
        for a=1 to 256: Filesizes(a)=0 : next a : rem zero all file sizes.

        slots=0 : rem Slots = free directory entries.

        for root=0 to AMDirectory-1

                DISKALLOCATION=GetAllocation(DISK,root) : rem Get hold of the disk allocation for the directory...
rem                DISKALLOCATION=DISKALLOCATION+GetAllocation(DISK,root+1)        : rem Two clusters for the directory = 64 max filenames per disk.
rem  Why did I do that? Is it an artifact?
rem I need to check the maths here... Allocations can vary in size...

                dircount=1

                CycleDirectory:
                notfound=1

                rem if mid$(DISKALLOCATION,dircount,1) = chr$(0) then   'we have a file - E5 or 229 is the "Unused Directory Marker"                            : rem No 229 is supposed to be CPM but there's a lot of crap in Amstrad disks.

                if mid$(DISKALLOCATION,dircount,1) <> chr$(229) then    : rem we have a file - E5 or 229 is the "Unused Directory Marker"

                        DISKFILENAME=mid$(DISKALLOCATION,dircount+1,8)+"."+mid$(DISKALLOCATION,dircount+9,3)
                        EXTENT=(asc(mid$(DISKALLOCATION,dircount+12,1)) mod 16) + (asc(mid$(DISKALLOCATION,dircount+14,1)) mod 32)

                        S1=asc(mid$(DISKALLOCATION,dircount+13,1)) mod 128                                                      : rem Mask the 8th bit ( bit 7 )
                        if S1=0 then S1=128                                                                                                                     : rem Because for S1, 0=128. Apply CP/M rules.
                        RC=asc(mid$(DISKALLOCATION,dircount+15,1))                                                                      : rem DONT Mask the 8th bit ( bit 7 ) because 80 means a FULL EXTENT - ie, ALL RECORDS TAKEN, and 0 means 0.
                                                                                                                                                                                : rem though for RC, zero should not happen. It means a zero length file.  No allocations.

						IF RC=0 then RC=128										: rem 0=128 and 128=128 for CP/M 2, 2.2 and 3 compatability. 
                        FILELENGTH=((RC-1)*128)+(S1)                                                                                     						: rem Now it's easy to calculate the file size of the extent.
REM this seems to cause bugs. If it's 0, then it's 128.                        if RC=0 then FILELENGTH=0                                                                                                       : rem shouldn't happen, but let's not work with negative numbers.
rem print "File:";DISKFILENAME;"   S1";S1;"   RC:";RC;"    L:";Filelength


						if asc(mid$(DISKALLOCATION,dircount+15,1))>127 then FILELENGTH=16384
						rem Nope. I can't remove the above line. It's a compatibility issue. If it's 80H or higher, the extent is full. 

                        if numfiles > 0 then

                                for DirSearch = 1 to numfiles
                                        if mid$(DISKALLOCATION,dircount+1,11)=mid$(files(DirSearch),2,11) then
                                                        notfound=0      : rem File was found so we added more clusters.
                                                        filesizes(Dirsearch)=filesizes(Dirsearch)+FILELENGTH
                                                        BuildFAT(Dirsearch)             : rem Make up FAT tables by recording used allocations.
                                                        Files(Dirsearch)=Files(Dirsearch)+mid(DISKALLOCATION,dircount,32) : rem Append subsequent file entries in full so we can deal with the file later, extent by extent.
                                        else
                                                rem Nothing Else yet.
                                        endif
                                next DirSearch

                        else

                                        rem THIS IS THE FIRST FILENAME. No need to search if it's already there. It's not.
                                        numfiles=1
                                        files(1)=mid(DISKALLOCATION,dircount,32)        : rem write first filename entry into file list.
                                        notfound=0      : rem it was found, because it was the first file we encountered.
                                        filesizes(1)=FILELENGTH
                                        DirSearch=1     : rem Manually set the Directory Search number for File 1. (since we don't loop in this version)
                                        BuildFAT(Dirsearch)             : rem Make up FAT tables by recording used allocations.

                        endif

                else
                        slots=slots+1   : rem record a directory slot ( extent ) is available.  Slots = free extents.
                endif

                if notfound=1 and mid$(DISKALLOCATION,dircount,1) <> chr$(229) then
                        rem debug       print "New Filename Found. Adding "; mid$(DISKALLOCATION,dircount+1,8);".";mid$(DISKALLOCATION,dircount+9,3)
                                numfiles=numfiles+1     : rem New File Found as the file was not found in the directory list we scanned.
                                files(DirSearch)=mid(DISKALLOCATION,dircount,32)        : rem write new filename into file list.
                                filesizes(Dirsearch)=FILELENGTH                                 : rem Record the length
                                BuildFAT(Dirsearch)             : rem Make up FAT tables by recording used allocations.
                                notfound=0      : rem it was found, because it was the first file we encountered.

                endif

                dircount=dircount+32
                if dircount < AMActualBS then goto CycleDirectory       : rem Repeat while there's still space in the allocation....

        next root


        rem Directory has now been built.
        rem Now calculate Free Space.

        FREESPACE=0

        for a=AMDirectory to allocs     : rem Don't count directory spaces as free. No need to add 1 to directory. It's already from 1.
                if FAT(a)=0 then freespace=freespace+AMActualBS         : rem If a free block, then add the block size to free space.
        next a

        rem     ShowFat : rem Show FAT tables in tabular form.

return 0

End Function

Function Getdrive (Byref resource as string) as string                                  : rem Resource is the filename, either short, or with a drive specifier.  Return just the file specifier.

        dim filespec as string
        dim drivenum as integer
        dim filename as string

        rem Opens the correct drive images based on either defaultdrive or the specified drive.

rem     print "Resource:";resource

        if mid(resource,2,1)=":" then                                   : rem Do we have a situation with a drive specifier?
                drivenum=asc(left$(resource,1))-64
                if drivenum>32 then drivenum=drivenum-32
                filespec=right(resource,len(resource)-2)
rem             Print "Assigned Drive:";drivenum
        else
rem             Print "Selecting Default Drive:";defaultdrive
                drivenum=defaultdrive                                           : rem Otherwise we're using the "Default" drive - Usually the logged drive.
                filespec=resource
rem             Print "Default Drive:";drivenum
        endif

        if filespec="" or filespec=chr(13) then
rem             Print "Selecting Default Resource:";defaultfname
                filespec=defaultfname
        endif

        defaultfname=""                                                                 : rem Make sure we only default the filename once. If we haven't used it by now, we're not going to use it. ( make sure this is after the filespec=default line )
        filespec=CASEFIX(filespec)                                              : rem And now we have Filespec ( filename ) down, let's fix the case in case it holds any lower case letters. Not sure if I want to change this later.

        if drivenum < 1 or drivenum > 16 then goto GetDriveError                : rem Drive number out of bounds.
rem        if DSKFILE(drivenum+1)="" then goto GetDriveError2                      : rem No image selected for that drive. Why the Plus1?
        if DSKFILE(drivenum)="" then goto GetDriveError2                      : rem No image selected for that drive.

rem     Print "Loading Drive:";drivenum


        DISK=LoadFile(DSKFILE(drivenum))                                : rem Load in the disk image into DISK.

        INIT(DISK)                                                                              : rem Initialise based on Disk Image.

        BuildDirectory(DISK)                                                    : rem Build Directory on current image initilised.

        goto DSKEXIT
        GetDriveError:
        print "Invalid Drive Selection." : SERROR=SERROR+1: goto DSKEXIT : rem make no change.
        GetDriveError2:
        print "No image mapped to drive requested."
rem     print "Drivenum:";drivenum,dskfile(drivenum)
        SERROR=SERROR+1 :  goto DSKEXIT
DSKEXIT:
rem Done here.

        rem DONT DO THIS HERE.... filespec=CleanFileName(filespec)                              : rem Let's clean up the filename as necessary first.

return filespec
End function

Function Putdrive (Byref image as string, Byref drivenum as integer) as integer

        Savefile(DefaultDSK)

Return 0
End Function

Function DeleteFile (Byref Fname as string) as integer                                                                  : REM XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX Currently under construction.

        rem DELETE FILE.
        rem
        rem Need a READ and WRITE allocation routine. ( have read allocation OK ).
        rem Need an allocation parse routine to find the files in the allocation.
        rem Starts like DIR but then checks the directory allocations.

        dim searchfile as string                        : rem the file name we are searching for.

        searchfile=GetDrive(fname)                                                                                                                              : rem Let's load the correct image, clean up the filename we want to delete and start building the extent.
        if searchfile="" then goto DeleteError1

        rem LETs check if what we have here is a valid filename, including any wildcards.

                print "Searching for file;"

                for a=0 to AMDirectory-1
                        DISKALLOCATION=GetAllocation(DISK,a)

                        for b=0 to AMActualBS-32 step 32

                        if matchfilename(searchfile,mid(DISKALLOCATION,b+2,11))=1 and mid(DISKALLOCATION,b+1,1) <> chr(229) then
                                print "Deleting:";mid(DISKALLOCATION,b+2,11)
                                insert (DISKALLOCATION,chr(229),b+1)            : rem Mark file as erased.
                        endif
                        next b

                DISK=putallocation(DISK,DISKALLOCATION,a)                       : rem Update disk image...

                next a                                                                                  : rem Next Allocation.

        savefile(DefaultDsk)

DeleteOK:
return 0

DeleteError1:
        Print "Error: No filename specified."
        goto DeleteOK

End function





Function RenameFile (Byref Fname as string,Byref Newname as string) as integer                                                                  : REM XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX Currently under construction.

        rem Rename File
        rem Modified from Delete File. Locates Filename and instead of writing it out, writes in the new name.
        rem Need a READ and WRITE allocation routine. ( have read allocation OK ).
        rem Need an allocation parse routine to find the files in the allocation.
        rem Starts like DIR but then checks the directory allocations.

        dim searchfile as string                        : rem the file name we are searching for.
        dim replacement as string                       : rem Replacement file name.
        dim localloop as integer                        : rem For cleaning up incoming file name into an 11 character string.

        searchfile=GetDrive(fname)                                                                                                                              : rem Let's load the correct image, clean up the filename we want to delete and start building the extent.
        if searchfile="" then goto DeleteError1

        replacement=""
        for localloop=1 to len(Newname)
                rem LITERAL HERE. DON"T DO ANY CHECKS ON FILE NAME. MIGHT PUT IN A DOT REMOVER IF ANYTHING LATER.
                replacement=replacement+mid(Newname,localloop,1)
        next localloop

        rem LETs check if what we have here is a valid filename, including any wildcards.

                print "Searching for file:";FNAME;" to rename as ";NEWNAME

                for a=0 to AMDirectory-1
                        DISKALLOCATION=GetAllocation(DISK,a)

                        for b=0 to AMActualBS-32 step 32

                        if matchfilename(searchfile,mid(DISKALLOCATION,b+2,11))=1 and mid(DISKALLOCATION,b+1,1) <> chr(229) then
                                print "Renaming:";mid(DISKALLOCATION,b+2,11);" to ";cleanfilename(Newname)
                                insert (DISKALLOCATION,cleanfilename(replacement),b+2)            : rem Rewrite file name with new file name. Do not erase or mark as erased.  Clean the filename first
                        endif
                        next b

                DISK=putallocation(DISK,DISKALLOCATION,a)                       : rem Update disk image...

                next a                                                                                  : rem Next Allocation.

        savefile(DefaultDsk)

DeleteOK:
        return 0

DeleteError1:
        Print "Error: No filename specified."
        goto DeleteOK

End function


Function Putfile (Byref image as string, Byref file as string, Byref fname as string) as string

                        rem This routine write a CPM file into a DSK file, so that it can be read by a CPM machine.
                        rem Needs to build FAT and other aspects.
                        rem Returns the image that has been updated with the file, under the new filename.

        dim newimage as string  : rem new image to be returned.
        newimage=image
        dim onealloc as integer : rem Temporarily holds an assigned allocation.                                                                         : rem Do I really need to know this in sectors here? Probably not.
        dim allcount as integer : rem Allocaitons we need to write for this file.
        dim excount as integer  : rem How many extents are we writing?
        dim count128    as integer      : rem How many 128s in this instance.
        dim part128 as integer  : rem How much of the last 128 do we use? ( Let's make this CP/M 3 compatible )
        dim extents as integer  : rem How many extents do we need? 1? 2? Count here. Don't confuse with extent which is a string.
        dim extent as string    : rem Create the extent record with the appropriate filenames, buts and pointers.
        dim fsearch as integer  : rem Counts through the file names
        dim exfat as string             : rem When we start putting together the FAT, load it here, before sticking it into extents.
        dim entries as integer  : rem How many file entries (extents) do we need? Do we have enough space in the directories?
        dim chunk as integer    : rem Start of current file chunk to be written to allocation

        rem Entries for the Extent Directory.
        dim EX as integer               : rem MOD 32 is the extent counter (low)
        dim S1 as integer               : rem Reserved 2.2...  Otherwise it's a sub-record indicator of the final record. Low bytes, so to speak.
        dim S2 as integer               : rem HIGH byte of the extent counter.
        dim RC as integer               : rem Record Counter

                extent=GetDrive(fname)                                                                                                                          : rem Let's load the correct image, clean up the filename and start building the extent.
                extent=CleanFilename(extent)                                                                                                            : rem Let's clean up the file name and start building the extent.

                rem We need to change drives and initialize with the above, before we do the below calculations, as allocations and block sizes change.

        rem Lets do some checks to make sure we can write this file before we start.
                if len(file)>freespace then print "Error, insufficient space on disk." : goto putfail                                           : rem Do we have enough free space?
                if len(file)<1 then print "Error, File length zero." : goto putfail
                if numfiles>254 then print "Error, No more filenames available." : goto putfail                                                         : rem Have we exceeded the maximum number of filenames?
                entries=int((len(file)-1)/AMActualBS)+1         : rem Calculate how many allocations (blocks) we need for this file.
                extents=int((entries-1)/(16/filepointers))+1            : rem Calculate how many extents are needed for this file.

                PRINT "We need :";extents;" extents for this file and";entries;" Allocations since it's ";len(file);" bytes long."

                if extents > slots then print "Error, Insufficient directory space." : goto putfail             : rem Do we have enough free extents?

        rem OK, we seem to have enough space to write this file. Let's clean up the filename, and replace any reserved characters.

        rem Do we have to load a new disk, and understand it's format and directories? Let's check.
        rem Write this next.

rem debug Print "Cleaned Filename:";extent

                fsearch=1
PutFileSearch:
                if extent = mid$(files(fsearch),2,11) then print "Error: File already exists" : goto Putfail            : Rem File Name Found. - Later have to make this "Overwrite? (Y)es/(N)o/(A)ll/(Q)uit"
                fsearch=fsearch+1                                                                                                                                                                       : Rem Later make File Search a function, including allowing multiple matches sequentially. Call Elsewhere too.
                if fsearch<=numfiles then goto PutFileSearch

        rem If we get here, file not found. We can write it. Let's expand the extent.
rem debug       Print "File not found - sufficient space - Continuing to write."        : rem remove later. Debug.

                extent=chr(0)+extent                                            : rem Add 0 to the start as a file marker.
rem debug       print "Writing";len(file);" bytes as ";entries;" allocations in";extents;" extents with ";filepointers;" bytes per allocation pointer"

        chunk=1
        files(numfiles+1)=""    : rem Clear out file data, in case it holds something else.

                for excount=1 to extents                                                                                : rem When we get to excount=extents then we need to change what we're writing in the file header.

                        EX=(excount mod 32)-1                                                                           : rem EX starts with 0, while our loop starts with 1.
                        S2=int(excount/32)


                        if excount = extents then
                                rem Last Extent. Work out file sizes for partial extent.
                                S1=len(file) mod 128
                                RC=int(((len(file)-1) mod 16384)/128)+1                                                                                                 :rem FOUND BUG. Have to add +1, because 0=80 (0 to 128 bytes) and if it's 128, it must be 80.

                                extent=left(extent,12)+chr(EX)+chr(S1)+chr(S2)+chr(RC)

                                rem We need to ZERO out the existing directory entries first....

                                for allcount = 1 to entries
                                        onealloc=SetNextAllocation(numfiles+1)
                                        extent=extent+chr(onealloc mod 256)
                                        if filepointers=2 then extent=extent+chr(int(onealloc/256))

                                        DISK=PutAllocation (DISK,mid(file+string(AMActualBS," "),chunk,AMActualBS),onealloc)                                            : rem Write the allocation to the disk. Pad the file for a full allocation
                                                                                                                                                                                                                                                                : rem Because otherwise it won't get data at the end of the file.

rem print "Writing Chunk at:";chunk;" for ";AMActualBS;" bytes."
rem print mid(file,chunk,AMActualBS)

                                        chunk=chunk+AMActualBS
                                next allcount

                                extent=left(extent+string(16,0),32)                                                                                                     : Rem Make sure any remaining extent entries are zeroed out. Then make sure extent length is only 32 bytes.

rem debug                               printhex (extent,len(extent))

                                PutDirEntry(extent)                                                                                                                                             : rem Write the file extent into the directory too.

                                files(numfiles+1)=files(numfiles+1)+extent                                                                                              : rem Write it to the file list.
                        else
                                rem Fill the extent.
                                S1=0
                                RC=128
                                extent=left(extent,12)+chr(EX)+chr(S1)+chr(S2)+chr(RC)
                                for allcount = 1 to (16/filepointers)
                                        onealloc=SetNextAllocation(numfiles+1)
                                        extent=extent+chr(onealloc mod 256)
                                        if filepointers=2 then extent=extent+chr(int(onealloc/256))

                                        DISK=PutAllocation (DISK,mid(file+string(AMActualBS," "),chunk,AMActualBS),onealloc)                                            : rem Write the allocation to the disk.  Pad the file for a full allocation
                                                                                                                                                                                                                                                                : rem Because otherwise it won't get data at the end of the file.

                                        entries=entries-1                                                                                                                                       : rem one less allocation to write.
                                        chunk=chunk+AMActualBS                                                                                                                          : rem Update the file pointer of what part we're writing.
                                next allcount

                                PutDirEntry(extent)                                                                                                                                             : rem Write the file extent into the directory too.


rem debug                               printhex (extent,len(extent))


                                files(numfiles+1)=files(numfiles+1)+extent                                                                                              : rem Write it to the file list.
                        endif


                next excount

        numfiles=numfiles+1
        filesizes(numfiles)=len(file)

rem debug                       print "What does DISK show?";: printhex (mid(DISK,((10*512)+257),32),32)

        rem Function PutAllocation (Byref image as string, Byref filedata as string, Byref allocation as integer) as string

                rem Need to check exact file name against directory. Make sure there's no wildcards.

                rem 1st Check Filename and number of spare allocations in FAT so we know we can write it.
                rem 2nd Locate Unused Allocations in FAT.
                rem 3rd Break up file into blocks and write allocations. ( putallocation is working and tested ).
                rem 4th Update Directory.                                                                ( putallocation is working and tested ).
                rem Also need to create a FAT entry, and have a way to write this back to the directory allocations.

        rem LAST thing to do, is write the file disk image back to the original. .

        savefile(DefaultDsk)

Putfail:
return DISK             : rem Zero = no success. We don't need to return anything if we write the disk.
End function

Function Getfile (Byref fname as string) as string

        dim exname as string    : rem expanded file name.
        dim decp as integer             : rem location of decimal point.
        dim ofile as string             : rem Where are we going to stick the output file as we build it?
        dim records as integer  : rem We need to know how many 128byte records are in the allocation.
        dim lastbytes as integer        : rem We need to know how many bytes of the last record are valid ( records are 128 bytes. Allocations are 80H x records = 16k )
        dim cumulativebytes as integer  : rem How may bytes long file is so we can truncate it later.

        rem print "Looking out for love... Or ;";fname
        rem NOTE - For practical reasons, requires an EXACT file name. Let's even keep it case sensitive.


        exname="           " : rem 11 blank characters. Let's clean up and format the file name with padding.
        decp=instr(fname,".")
        if decp=0 then
                        exname=fname+exname
                        exname=left$(exname,11)
        else
                        exname=mid$(fname,1,decp-1)+"        "
                        exname=left$(exname,8)
                        exname=exname+mid$(fname,decp+1,3)+"   "
                        exname=left$(exname,11)
        endif
        rem - I might have to make a clean up routine some day to mask bit7 of the filenames that are stored.
        rem See if I can use the CleanFilename function later, since it came from here.

        a=1
GetFileSearch:
        if exname = mid$(files(a),2,11) then goto Getfilefound          : Rem File Name Found.
        a=a+1
        if a<=numfiles then goto GetFileSearch

        rem If we get here, file not found.
        Print "File not found."
        ofile=""
        goto GetFileExit

GetFileFound:

        FILEENTRY=files(a)      : rem Store file directory entry....

rem debug       PRINTHEX (FILEENTRY,len(FILEENTRY))
rem debug       print

cumulativebytes=0

        for a=1 to len(FILEENTRY)/32    : rem Once for each extent.

                records=asc(mid(FILEENTRY,((a-1)*32)+16,1))
                if records=0 then records=128
                lastbytes=asc(mid(FILEENTRY,((a-1)*32)+14,1))
                if lastbytes=0 then lastbytes=128
                cumulativebytes=cumulativebytes+((records*128)+lastbytes-128)                                                                           : rem Subtract 128 bytes so we don't repeat the last record twice. ( We count it in the sub counter ).

                rem debug Print "Allocation Records:";records;"    last-record-bytes:";lastbytes;"     so total bytes in this allocation:";(records*128)+lastbytes-128;"      Cumulative:";cumulativebytes;"                 Filepointers:";filepointers

                for b=1 to 16
                fatallocs=asc(mid(FILEENTRY,((a-1)*32)+16+b,1))
                if filepointers=2 then b=b+1: fatallocs=fatallocs+(asc(mid(FILEENTRY,((a-1)*32)+16+b,1))*256) : rem Add in upper pointer to allocation.
                if fatallocs <> 0 then ofile=ofile+getallocation(DISK,fatallocs)                                                                                : rem I have to limit this to the size specified if less than an allocation. At the moment I'm loading the entire allocation into OFILE
                if fatallocs=0 then b=16
                next b

        next a



rem     print mid$(files(a),2,8);".";mid$(files(a),10,3);"   ";filesizes(a);" Bytes"
rem     print FREESPACE;" bytes free in ";FREESPACE/AMActualBS;" allocations."


GetFileExit:

rem debug print "File Retrieved, Total Size:";len(ofile)

ofile=left(ofile,cumulativebytes)       : rem Clip size of Ofile.

rem debug print "Adjusted new file size:";len(ofile)

return ofile

End function





Function fileattributes (Byref fname as string) as string
rem Modified Getfile routine.

        dim exname as string    : rem expanded file name.
        dim decp as integer             : rem location of decimal point.
        dim ofile as string             : rem Where are we going to stick the output file as we build it?
        dim records as integer  : rem We need to know how many 128byte records are in the allocation.
		dim lastbytes as integer        : rem We need to know how many bytes of the last record are valid ( records are 128 bytes. Allocations are 80H x records = 16k )
        dim cumulativebytes as integer  : rem How may bytes long file is so we can truncate it later.

        rem print "Looking out for love... Or ;";fname
        rem NOTE - For practical reasons, requires an EXACT file name. Let's even keep it case sensitive.


        exname="           " : rem 11 blank characters. Let's clean up and format the file name with padding.
        decp=instr(fname,".")
        if decp=0 then
                        exname=fname+exname
                        exname=left$(exname,11)
        else
                        exname=mid$(fname,1,decp-1)+"        "
                        exname=left$(exname,8)
                        exname=exname+mid$(fname,decp+1,3)+"   "
                        exname=left$(exname,11)
        endif
        rem - I might have to make a clean up routine some day to mask bit7 of the filenames that are stored.
        rem See if I can use the CleanFilename function later, since it came from here.

        a=1
AGetFileSearch:
        if exname = mid$(files(a),2,11) then goto aGetfilefound          : Rem File Name Found.
        a=a+1
        if a<=numfiles then goto AGetFileSearch

        rem If we get here, file not found.
        Print "File not found."
        ofile=""
        goto AGetFileExit

AGetFileFound:

        FILEENTRY=files(a)      : rem Store file directory entry....

rem debug       PRINTHEX (FILEENTRY,len(FILEENTRY))
rem debug       print

cumulativebytes=0

		print "Extents in file (Entries in the file tables):";int(LEN(FILEENTRY)/32)
        for a=1 to len(FILEENTRY)/32    : rem Once for each extent.

                records=asc(mid(FILEENTRY,((a-1)*32)+16,1))
                if records=0 then records=128
                lastbytes=asc(mid(FILEENTRY,((a-1)*32)+14,1))
                if lastbytes=0 then lastbytes=128
                cumulativebytes=cumulativebytes+((records*128)+lastbytes-128)                                                                           : rem Subtract 128 bytes so we don't repeat the last record twice. ( We count it in the sub counter ).


rem Here's where we print it. 
                Print "Extent:";a;"  128byte Blocks(records):";records;"  last-record-bytes:";lastbytes;"  Total bytes in this extent:";(records*128)+lastbytes-128;"  Cumulative:";cumulativebytes;"  Pointer-Bytes:";filepointers
				Print "Allocations in extent"; 

                for b=1 to 16
                fatallocs=asc(mid(FILEENTRY,((a-1)*32)+16+b,1))
                if filepointers=2 then b=b+1: fatallocs=fatallocs+(asc(mid(FILEENTRY,((a-1)*32)+16+b,1))*256) : rem Add in upper pointer to allocation.

rem                if fatallocs <> 0 then ofile=ofile+getallocation(DISK,fatallocs)                                                                                : rem I have to limit this to the size specified if less than an allocation. At the moment I'm loading the entire allocation into OFILE
rem We don't need to retrieve the file details. 
                if fatallocs <> 0 then print " - "; fatallocs;                                                                    
                if fatallocs=0 then b=16
                next b
				print

        next a



rem     print mid$(files(a),2,8);".";mid$(files(a),10,3);"   ";filesizes(a);" Bytes"
rem     print FREESPACE;" bytes free in ";FREESPACE/AMActualBS;" allocations."


AGetFileExit:

rem debug print "File Retrieved, Total Size:";len(ofile)

ofile=left(ofile,cumulativebytes)       : rem Clip size of Ofile.

rem debug print "Adjusted new file size:";len(ofile)


return ofile

End function






function DRIVEOK(Byref text as string) as integer

dim tempa as integer
dim result as integer

        result=0                        : rem drive is mappable.
        rem Sanitize drive selector.
        tempa=asc(left$(text,1))
        if tempa>96 then tempa=tempa-32
        if tempa<65 then result=1
        if tempa>80 then result=2
        tempa=tempa-64
        if DSKFILE(tempa)="" then result=3

        return result
End function


function GETCHECKSUM(image as string) as integer
rem count out a checksum.

dim lchecksum as integer
dim ccount as integer

        lchecksum=0

        for ccount=1 to len(image)
                lchecksum=(lchecksum+asc(mid(image,ccount,1))) mod 256
        next ccount

        return lchecksum
end function

function FIDDLE(image as string,target as integer) as string
rem Change Checksum Byte in a string to make the result of the string's checksum equal the target checksum

dim newsum as integer   : rem What is the new check sum we need?

        if target > 255 or target < 0 then print "Error: Target checksum value should be in range of 0 to 255 or &h00 to &hFF " : SERROR=SERROR+1: goto fiddlex

        newsum=(256+target-GETCHECKSUM(mid(image,513,AMActualSS))) mod 256
        insert (image,chr(newsum),513+15)

        print "Changing Checksum to:";hex(newsum)

        return image
fiddlex:

end function


function nocrlf (byref text as string) as string
dim result as string
dim localloop as integer

result=""

for localloop=1 to len(text)
if mid(text,localloop,1)>" " then
        result=result+mid(text,localloop,1)
endif
next localloop

return result

end function


rem The functions below ( addxxx ) are used to build a disk image in memory for the purposes of
rem formatting a disk, or creating a disk image from scratch.

function getbytes (byref number as integer, byref length as integer) as string
rem Return a string of bytes equal to a number of a length indicated by the length.
rem Little Endian.

dim result as string
dim localloop as integer

        result=""
        number=number*256

        for localloop = 1 to length

                number=int(number/256)
                result=result+chr(number mod 256)

        next localloop

        return result
end function




function adddib (byref text as string) as string
rem Creates 256 bytes of DIB Disk Information Block from variables backwards.
REM DISK INFORMATION BLOCK.
REM TITLE    0-21 (1-34), 34 chars, Title of Disk File.        "MV - CPCEMU Disk-File\r\nDisk-Info\r\n"
REM CREATOR  22-2F (35-48), 14 chars, Creator Name
REM TRACKS   30 (49), 1 byte
REM SIDES    31 (50), 1 byte
REM TRACKLEN 32 (51), 2 bytes
REM UNUSED   34 (53), 204 bytes

dim result as string
dim crlf as string

        crlf=chr(13)+chr(10)

        rem    "MV - CPCEMU Disk-File\r\nDisk-Info\r\n"
        result="CPM DiskImage Manager"+crlf+"Disk-Info"+crlf
        result=result+"David Kitson  "
        result=result+getbytes(ftracks,1)
        result=result+getbytes(fheads,1)
        result=result+getbytes(fsectors*fsectsize,2)
        result=result+string(204,0)

        print "Length of DIB:";len(result)


        result=text+result
        return result

end function




function addtib (byref text as string) as string
rem Creates 26 bytes of TIB Track Information Block from variables backwards.
rem TRACK INFORMATION BLOCK.
REM TTITLE     0-0c (1-13), 13 chars, Title of Tracks. "Track-Info\r\n"
REM UNUSED2    0d-0f (14-16), 3 chars, Unused.
REM TRACKNUM   10 (17), 1 byte.
REM SIDENUM    11 (18), 1 byte.
REM UNUSED3    12-13 (19), 2 bytes.
REM Sectorsize 14 (21), 1 byte.
REM Numsectors 15 (22), 1 byte.
REM GAP3       16 (23), 1 byte. Not sure what this is?
REM Filler     17 (24), 1 byte. Not sure what this is?
REM SectorInfo 18-FF (25-256), 232 bytes. Sector Information List. Note: Track Info
REM                            Block takes up 256 bytes...

dim result as string
dim crlf as string
        crlf=chr(13)+chr(10)

        result="Track-Info"+crlf+chr(0)
        result=result+string(3,0)
        result=result+getbytes(FTRACKNUM,1)
        result=result+getbytes(FSIDENUM,1)
        result=result+string(2,0)
        result=result+getbytes(FSECTSIZE,1)
        result=result+getbytes(FSECTORS,1)
        result=result+string(1,0)
        result=result+string(1,0)
        result=result+string(232,0)

        print "Length of TIB:";len(result)

        result=text+result
        return result

end function


function addamstrad (byref text as string) as string


rem Creates 16 bytes of Amstrad Identifier in Track0, Sector 0.
rem DISKIDENTIFIER 16 bytes of Disk Specification Identifier - This is AMSTRAD data to
rem    tell the computer what the disk is, not just the virtual DSK disk.
rem AMType      Byte 0, 0=standard DD DS ST +3 disk. 1=CPC DD SS ST system. 2= CPC DDv
rem                      SS ST Data Only Format, 3 = Standard DD DS DT disk (+3 compat )
rem AMSides     Byte 1, 0 = Single Sided, 1 = Double Sided Interleave, 2 = Side 1
rem                      then Side 2 in that order... Or maybe for more sides.
rem AMTracks    Byte 2, Number of tracks per side.
rem AMSectors   Byte 3, Number of Sectors per track.
rem AMSectSize  Byte 4, Sector Size ( Log2 - 7 ) - So 2 = 2^9 = 512 bytes / sector.
rem                      1 = 256 and 0 = 128. Should never be that small.
rem AMActualSS           Calculated Sector size from Byte 4.
rem AMReserved  Byte 5, Number of reserved tracks. Tracks that aren't DISK contents,
rem                      or are System data.
rem AMBlockSize Byte 6, Block Size (Log2(Blocksize/128)) - Allocations - So 3 = 8 *
rem                      128 = 1024K. This is the ALLOCATION SIZE. ( why should it be 4? need to check later )
rem AMActualBS           Calculated Block ( allocation ) size from Byte 6.
rem AMDirectory Byte 7, How many allocations are DIRECTORY allocations?
rem AMGap       Byte 8, Gap Length for read/write.
rem AMGap2      Byte 9, Gap Length for Format.
rem AMUnused    Bytes 10 to 14. Currently reserved.
rem AMChecksum  Byte 15, Used for bootable disk only.

dim result as string
dim amdata as string
dim persect as integer
dim blocksize as integer
dim maxallocs as integer	: rem How many allocations do we need?

result=text

rem debug
rem print "TracksXX:";FTRACKS,FSIDENUM,FSECTSIZE,FSECTORS



if (FTRACKS*FSECTORS*FSIDENUM)/2 > 256 then
			rem - Will happen with bigger disks, so need a 2 byte pointer to allocations.
		blocksize=4
else
		blocksize=3
endif


amdata=chr(4)             : rem Not Amstrad type. But could be.
amdata=amdata+getbytes(FHEADS MOD 2,1) : rem Interleave heads with tracks. Can't add more than 2 sides here.
amdata=amdata+getbytes(FTRACKS,1)         : rem Lets make it 255 tracks per side. Make a Big disk.
amdata=amdata+getbytes(FSECTORS,1)        : rem Let's make it 255 sectors/track.
amdata=amdata+chr(2)      : rem 512k per sector.
amdata=amdata+chr(1)      : rem 1 reserved track.
amdata=amdata+chr(blocksize)      : rem 3=1024K block size. 4=2048K block size. 
amdata=amdata+chr(4)      : rem Four directory allocations.
amdata=amdata+chr(40)     : rem Random gap size.
amdata=amdata+chr(40)     : rem Random gap size.
amdata=amdata+string(5,0) : rem Reserved. Should be zero.
amdata=amdata+chr(0)      : rem Check sum. Fiddle later if required.

INSERT (result,amdata,&h201) : rem File location 200H is position 201H in string.

return result
end function






function addtrack (byref text as string) as string
dim result as string


result=text+string(FSECTSIZE*FSECTORS,&hE5)

print "Tracklen:";len(result)

return result
end function





function parameter ( byref target as string, byref defaultpara as integer ) as integer
rem Works with what is in CLI - Examines for parameters without other factors in the form of AAA=NNNN with no spacing.
dim result as integer
dim localloop as integer
dim value as string     : rem where we collect the value.
dim hit as integer      : rem location in string.
dim CLEANCLI as string  : rem Cleaned casefix(ed) CLI

CLEANCLI=casefix(CLI)   : rem Clean up the case of the CLI.

        result=defaultpara  : rem Set the default value if we don't find it.
        value=""
        hit = instr(CLEANCLI,target+"=")

        if hit=0 then goto paraend2
        hit=hit+len(target)+1

paratop:
        if mid(CLEANCLI,hit,1) <"0" or mid(CLEANCLI,hit,1)>"9" then goto paraend
        value=value+mid(CLEANCLI,hit,1)
        hit=hit+1
        if hit <= len(CLEANCLI) then goto paratop

paraend:
        result=val(value)
paraend2:
        return result
end function


function justtext (byref text as string ) as string
rem Cleans a string of any non-text characters. Replace Control Codes with Space.
dim response as string
dim localloop as integer

        for localloop=1 to len(text)
                if mid(text,localloop,1)>chr(31) then
                        response=response+mid(text,localloop,1)
                else
                        response=response+" "
                endif
        next localloop
        return response
end function



Function ZXNumber (Byref text as string) as integer
dim result as integer

        if mid(text,1,1)=chr(0) then goto ZXINTEGER
        rem We can deal with non-integer numbers here later.
        goto ZXNumEnd

ZXINTEGER:
        if mid(text,4,1)<>chr(0) then goto ZXERROR
        result=asc(mid(text,2,1))+256*asc(mid(text,3,1))
        goto ZXNumEnd

ZXError:
        rem In the event of an error, make the response a zero.
        result=0
ZXNumEnd:
        return result
End function




Function ZXPRINT (Byref filecontents as string) as string
rem Print out a ZX BASIC file and convert tokenized characters to strings.
rem String replacement for tokens is stored in UZX array at start of program.

dim response as string
dim localloop as integer
dim asciichar as string
dim zxline as integer : rem line number
dim zxlen as integer : rem length of basic line.
dim zxpoint as integer : rem where are we in the program listing?
dim zxquote as integer  : rem 0 for not quoted, 1 for quoted.
dim zxnumeral as integer : rem 0 for not a number, 1 for a number.

        response=""
        zxquote=0
        zxnumeral=0

        zxpoint=1

        rem printhex (filecontents,len(filecontents)) : rem Just lets me see what is there.

ZXTOP:
rem Return here for each interation until file is read.
        zxline=asc(mid(filecontents,zxpoint,1))*256+asc(mid(filecontents,zxpoint+1,1))  : rem big endian line number.
        zxlen=asc(mid(filecontents,zxpoint+2,1))+256*asc(mid(filecontents,zxpoint+3,1))
        print ;zxline;"  ";     : rem Print line number and two spaces.

        for localloop=zxpoint+4 to zxpoint+3+zxlen
                asciichar=mid(filecontents,localloop,1)
                if asciichar=chr(34) then zxquote=(zxquote+1) mod 2
                if asciichar>="0" and asciichar <="9" then zxnumeral=(zxnumeral+1) mod 2
                if asciichar<"0" then zxnumeral=0
                if asciichar>"9" and asciichar <>"e" then zxnumeral=0

                if zxnumeral=1 and zxquote=0 and asciichar="e" then
                        rem Deal with exponent in text - eg, 1e5 should be 10000 -
                        localloop=localloop+1
                        print string ((asc(mid(filecontents,localloop,1)))-48,"0");
                        zxnumeral=0
                        asciichar=" "
                endif

                if asciichar>chr(31) and asciichar<chr(165) then
                        print asciichar;
                endif

                if asciichar>=chr(165) then
                        print " ";uzx(asc(asciichar));" ";
                endif

                if asciichar=chr(0E) then
rem             print "#";zxnumber(mid(filecontents,localloop+1,4));"#";
                        localloop=localloop+4   : rem Skip past rest of number.
                endif


        next localloop
        print

        zxpoint=zxpoint+zxlen+4

        if zxpoint<len(filecontents) then goto zxtop

return response
end function




Function TAPETRANS(Byref filename as string) as string
rem Return a list of files in the filename mentioned.
dim TFILE as string
dim TPOS as integer
dim result as string
dim localloop as integer
dim tseg as integer
dim fseg as string      : rem file segment.
dim header as integer   : rem 0 or FF - 0=header, FF=Content.
dim ptype as integer    : rem Program Type 0,1,2, or 3.
dim tname as string     : rem tape file name.

        TFILE=loadfile(filename)

        print len (tfile)

        for localloop=1 to len(tfile)

                tseg=asc(mid(tfile,localloop,1))+256*asc(mid(tfile,localloop+1,1))
                fseg=mid(tfile,localloop+3,tseg)
                header=asc(mid(tfile,localloop+2,1))

                if header=0 then
                        ptype=asc(mid(tfile,localloop+3,1))
                        tname=mid(tfile,localloop+4,10)
                else
rem                     print
                endif

                if header=255 then DISK=PutFile(DISK,FSEG,TNAME)                        : rem Write the tape file to disk.

                localloop=localloop+tseg+1
        next localloop

        return result
end function

Function TAPDIR(Byref filename as string) as string
rem Return a list of files in the filename mentioned.
dim TFILE as string
dim TPOS as integer
dim result as string
dim localloop as integer
dim tseg as integer
dim fseg as string      : rem file segment.
dim header as integer   : rem 0 or FF - 0=header, FF=Content.
dim ptype as integer    : rem Program Type 0,1,2, or 3.
dim tname as string     : rem tape file name.
dim pnum as integer     : rem Which file on tape.

        TFILE=loadfile(filename)
        pnum=1

        print len (tfile)

        for localloop=1 to len(tfile)

                tseg=asc(mid(tfile,localloop,1))+256*asc(mid(tfile,localloop+1,1))
                fseg=mid(tfile,localloop+3,tseg)
                header=asc(mid(tfile,localloop+2,1))

                if header=0 then
                        ptype=asc(mid(tfile,localloop+3,1))
                        tname=mid(tfile,localloop+4,10)
                else
                        rem Otherwise will be 255.
                        Print "Position:";pnum,justtext(tname),"type:";ptype,tseg;"bytes"
                        pnum=pnum+1
                endif

                localloop=localloop+tseg+1
        next localloop

        return result
end function











rem ###########################################################################################################
rem
rem ###########################################################################################################
rem
rem ###########################################################################################################
rem
rem ###########################################################################################################
rem
rem ###########################################################################################################
rem
rem SUBROUTINES
rem
rem ###########################################################################################################
rem
rem ###########################################################################################################
rem
rem ###########################################################################################################
rem
rem ###########################################################################################################
rem
rem ###########################################################################################################




sub RENAME
rem Renames a file given a filename.

        renamefile (argument(cli,2),argument(cli,3))

end sub


sub RENAMENUM
rem Rename is same as rename, but picks the file out by it's number. NOT Number is in HEX...
dim DESTNAME as string  : rem Destination file name
dim SOURCENUM as string : rem Let's prefix a 2 digit hex number with '&h' so we can val it.
dim SOURCEVAL as integer        : rem Once we have a VAL'able string, let's turn it into a number so we can look it up in an array.
dim SOURCENAME as string : rem The source name as a string.

DESTNAME=argument(cli,3)
SOURCENUM="&h"+argument(cli,2)
SOURCEVAL=val(SOURCENUM)
SOURCENAME=mid(FILES(SOURCEVAL),2,11)
print "Attempting to rename file:";hex(SOURCEVAL,2);"  (Decimal:";SOURCEVAL;")"
print "Associated Filename: [";CLEANFILENAME(SOURCENAME);"]"
print "Destination Filename:[";CLEANFILENAME(argument(cli,3));"]"
print

        renamefile (SOURCENAME,DESTNAME)


end sub


SUB UPPERZXCHAR
rem dim UZX(256) as string
dim localloop as integer

        for localloop=165 to 255
                read UZX(localloop)
        next localloop

        for localloop=165 to 255
                rem print localloop,uzx(localloop);
        next localloop

END SUB


SUB TAPECOPY
rem Load up the .TAP file and copy all of the files to the selected disk.
rem if casefix(comm)="TAPECOPY" then TAPDIR("brad.tap") : comm="null"

TAPETRANS (argument(cli,2))

END SUB


SUB TAPEDIR
rem Load up the .TAP file and copy all of the files to the selected disk.
rem if casefix(comm)="TAPECOPY" then TAPDIR("brad.tap") : comm="null"

TAPDIR (argument(cli,2))

END SUB

SUB ZXLIST

                                                                                : rem Display a file in hex. Might want to ASCII it later.
        sfile=getdrive(argument(CLI,2))                                                                 : rem temp store of the current filename in question if it exists.  Already cleaned. And Initialize the correct drive.
        rem - Note we're only expecting drive specifiers, otherwise we will just perform locally.

        dim localloop as integer

        for localloop=1 to numfiles
                        GetDrive(argument(CLI,2))                                                                                                                                               : rem Let's load the correct image, clean up the filename and start building the extent.
                        if      matchfilename(sfile,mid(files(localloop),2,11)) =1 then                                                 : rem If so, did it match the filenames picked out?
                                FILE=GetFile(mid(files(localloop),2,11))                                                                                : rem And now we get the exact filename...
                                defaultfname=mid(files(localloop),2,11)                                                                                 : rem Remember the current matched file name in case none is specified next.
                                Print "ZX BASIC FILE DECODE:";defaultfname
                                ZXPRINT(FILE)                                                                                 : rem NOW PRINT THE FILE we just read.
                        endif
        next localloop



rem ZXPRINT (FILE).

END SUB

sub TESTROUTINE
rem Just a test hook.
Print "Test Routine: Nothing to test. This is a debug command only."
goto TESTEND

ftracks=40
fheads=1
fsectors=9
fsectsize=512

adddib ("")
addtib ("")
addtrack ("")

TESTEND:
end sub


sub CopyBootDELETETHISLATER
rem Copy Boot Sectors.

                                                                                : rem Display a file in hex. Might want to ASCII it later.

        sfile=getdrive(argument(CLI,2))                                                                 : rem temp store of the current filename in question if it exists.  Already cleaned. And Initialize the correct drive.
        rem - Note we're only expecting drive specifiers, otherwise we will just perform locally.

        dim localloop as integer

        for localloop=1 to numfiles
                        GetDrive(argument(CLI,2))                                                                                                                                               : rem Let's load the correct image, clean up the filename and start building the extent.
                        if      matchfilename(sfile,mid(files(localloop),2,11)) =1 then                                                 : rem If so, did it match the filenames picked out?
                                FILE=GetFile(mid(files(localloop),2,11))                                                                                : rem And now we get the exact filename...
                                defaultfname=mid(files(localloop),2,11)                                                                                 : rem Remember the current matched file name in case none is specified next.
                                Print "Copying:";defaultfname
                                DISK=PutFile(DISK,FILE,argument(CLI,3))                                                                                 : rem NOW WRITE THE FILE we just read.
                        endif
        next localloop


end sub



sub ShowFat
rem Pictorially illustrates the FAT space, showing which filename is using which allocation. Typically directory allocations show as not used.
rem Later I might use a non-HEX character to identify where a directory is.
rem eg, DR DR DR DR instead of -- -- -- --
rem FAT is an array, so allocation 0 starts at position 1. Always add 1 to allocs.

        print
        print "Fat Tables:"
        print "     ";
        for a=0 to 15
                print hex(a,2);
                if a<15 then print "-";
        next a
        print
        for a=0 to 4095
                print hex(a*16,4);"-";
        for b=0 to 15
        if fat(b+(a*16)+1) <> 0 then
                print hex(fat(b+(a*16)+1),2);" ";:
        else
                if (a*16)+b < AMDirectory then
                        print "DR ";
                else
                        print "-- ";
                endif
        endif

        if (b+(a*16)+1) > allocs then
                b=15
                a=4095
        endif

        next b
        print
        next a
        Print "DR=Directory Allocation"

end sub




sub Dirnum

        print
        print "Files Found and Sorted;"
        for a=1 to numfiles
        print hex(a,2);" ";justtext(mid$(files(a),2,8));".";justtext(mid$(files(a),10,3));"   ";filesizes(a);" Bytes"
        next a
        print FREESPACE;" bytes free in ";FREESPACE/AMActualBS;" allocations."

end sub



sub Xrenum

        print
        print "Files Found and Sorted;"
        for a=1 to numfiles
        print hex(a,2);" ";mid$(files(a),2,8);".";mid$(files(a),10,3);"   ";filesizes(a);" Bytes"
        next a
        print FREESPACE;" bytes free in ";FREESPACE/AMActualBS;" allocations."

end sub



sub STAT
rem Print out Stat Information.

        print "Disk Information on current image."
        print
        Print "Disk Image Information:"
        Print "Title:";TITLE
        Print "Creator:";CREATOR
        Print "Tracks:";TRACKS
        Print "Sides:";SIDES
        Print "Track Length:";TRACKLEN
        Print "Unused:";UNUSED
        Print "Track Data Length:";len (DISKTRACK) - 256
        Print "---------------------------"
        Print "Total Number of tracks on disk:";TOTALTRACKS
        Print "---------------------------"
        Print "Reading Track 0"
        Print "Track Title:";TTITLE
        Print "Track Number:";TRACKNUM
        Print "Side Number:";SIDENUM
        Print "Sectorsize:";Sectorsize*256
        Print "Number of Sectors on track:";Numsectors
        Print "GAP3:";GAP3
        Print "Data bytes per Track:";Sectorsize*Numsectors*256
        Print
        Print "---------------------------"
        Print " Reading Disk Sector 0, Track 0, Disk Identifier"

        if AMDefault=0 then print " XDPB AMSTRAD/ZX+3 DISK. Extended Disk Parameter Block contents;" : print : printhex(XDPB,16)
        if AMDefault=1 then print " DEFAULT DISK PARAMETERS LOADED. UNKNOWN FORMAT"
        if AMDefault=2 then print " DEFAULT AMSTRAD SYSTEM DISK"
        if AMDefault=3 then print " DEFAULT AMSTRAD DATA DISK."

        print
        Print "Boot Sector Checksum:"; hex( getchecksum(getsect(DISK,1)) mod 256 )
        Print "01=PCW9xxx, 03=ZXSpectrum+3, FF=PCW8xxx"

        print

        Print "Track 0 Interleave as follows: (L=Logical. P=Physical)"
		for a=1 to AMSECTORS
		print "L";a;"=P";translate(a,0);" :: ";
		next a
		print 
		
		Print "I think this disk Is:"
        Print "DiskType:";AMType
        Print "Sides:";AMSides+1;"  (Byte1=0=1 side, Byte 1=1=2 sides)"
        Print "Tracks:";AMTracks
        Print "AMSectors:";AMSectors
        Print "AMSectSize:";AMSectSize
        Print "Actual Sector Size:";AMActualSS
        Print "AMReserved:";AMReserved
        Print "AMBlockSize:";AMBlockSize
        Print "Actual Allocation Size:";AMActualBS
        Print "AMDIrectory:";AMDirectory
        Print "AMGap:";AMGap
        Print "AMGap2:";AMGap2
        Print "AMChecksum:";AMChecksum
        Print "Sectors per Allocation:";SectorsPerAllocation

        rem Identify allocations and data size and other information.
        Print "Allocations:";allocs
        Print "Last Allocation:",hex(allocs,3)
        If allocs>255 then Print "Two bytes per allocation entry within extent."
        Print "Directory Allocations:";AMDirectory
        Print "Disk Data 'Formatted' Capacity:";(allocs-AMDirectory)*AMActualBS; " bytes"
        Print "Free Space:";FREESPACE; " bytes"
        Print "Available Directory Extents:";slots
        Print "Bytes per allocation pointer:";filepointers

end sub

sub HEXDIR
rem DUMP DIRECTORY ALLOCATIONS IN HEX... 32 bytes per line, full size of allocation.

rem  Show all of the directory allocations, in full size.

                print "Disc Directory Allocations:"; AMDirectory
                print

for a=0 to AMDirectory-1
        DISKALLOCATION=GetAllocation(DISK,a)
        PRINTHEX(DISKALLOCATION,AMActualBS)
next a

end sub



sub INITIALISE
rem Initialise Disk.... Read in the file and set the main variables.
rem Get a sector from the disk image so that you can read what it's all about. Generally at least.

        init(DISK)

        DIRECTORY       : rem Call Directory Subroutine, Build Directory.


end sub


sub help
        print "CP/M and Amstrad / Spectrum+3 Disk Image copier and tools."
        print "2022 David Kitson."
        print "Published under Creative Commons. No commercial use, attribution required. CC-BY-NC. May be distributed with other commercial software."
        print
        print "quit / exit / q         Quit this program."
        print "dir <filename>          Show a directory listing, with file sizes."
        print "dirnum <filename>       Show a directory listing with files numbered"
        print "dirhex <filename>       Dump directory allocations in hex. "
        print "delete filename         Delete a file of this name. Also accept del."
        print "Examine filename        Show details about a specific file"
        print "help                    This help page."
        print "stat                    Show disk status and other variables and information"
        print "sector n                Dump sector n as Hex"
        print "allocation n            Dump allocation m as Hex."
        print "dib                     Show DSK file Disk Information Block"
        print "tib n                   Show DSK file Track Information Block for track n"
        print "copy <fname> <fname>    Copy file a to file b. Includes disk identifier. Can copy between disks. Supports default destination filename and wildcards."
        print "bcopy <drive> <drive> n Copy boot sector from first drive to second drive and fudge with the checksum n."
        print "acopy <drv>n1 <drv>n2   Copy allocation file n1 on first drive to n2 on second drive. "
        print "scopy <drv>n1 <drv>n2   Copy allocation file n1 on first drive to n2 on second drive. "
        print "lcopy <local> <fname>   Copy local drive file to fname Supports default destination filename and wildcards."
        print "lput <fname> <local>    Copy fname to local drive. Supports default destination filename and wildcards."
        print "delete <filename>       Delete file a on currently selected disk"
        print "format                  Format currently selected disk. ## Not yet implemented."
        print "fat                     Show File Allocation Table usage. Not that CP/M uses FAT, but well, show allocation usage."
		print "interleave              Show the Interleave of the TIBs throughut the image. "
        print "type <filename>         Type out contents of file a"
        print "text <filename>         Same as type but filtered for printable ASCII with CRLF permitted."
        print "dump <filename>         Hex Dump file <filename>"
        print "typeascii a             Type out contents of file a in ASCII Limited. eg 7 bits, minimum control characters"
        print "mount filename drive    Mount image F as drive D eg, mount disk.dsk h: ## Not yet implemented."
        print "unmount drive           Unmount the image assigned to drive d - eg unmount a:"
        print "list                    List all mounted disk images."
rem     print "default9                If Disk Information Block is corrupted, then assume default 9 sectors config"
        print "hextrack n              Hexdump an entire track... Very long... For debugging. "
        print "slice n1 n2             Show in hex the current logged disk image file  starting at n1, for n2 bytes. "
        print "                         eg, slice &h200 128 would show from hex position &h200 the next 128 bytes. Can use HEX or DEC"
        print "rename <fname> <fname>  Rename a file."
        print "renamenum <num>         rename a file by file number - useful when a filename has non-printable characters or control codes."
        print "delnum <num>            Delete file number ( See filename numbers with DIRNUM command. )"
        print
        print "Local file functions:"
        print " ldir <filename>         Show the local directory and/or the filename. Supports absolute paths and relative paths."
        print " lcopy <fname> <fname>   Copy a file from the local host directory to a CP/M image directory."
        print " lput <fname>            Copy a file from CP/M directory to the local host directory."
        print " shell <argument>        Perform a shell command."
        print " save                    Save the current list of mounted disks"
        print
        print "ZX related functions:"
        print " tapecopy <localfile>    Copy from the local TAP file to the currently selected disk all files. eg, tapecopy game.tap"
        print " tapedir <localfile>     Show a numbered list of files from the specified local tap file."
        print " zxlist                  Show a headlerless BASIC file in ZX Tokenised basic as text."
        print " 3list                   Show a +3 formatted BASIC file in ZX Tokenised basic as text."
        print
        print "To change logged disk, type in the disk name as mapped in the config file. "
        print "eg,"
        print "A> C:                   type in C:"
        print "C>                      prompt changes to C: and now C: can be accessed and commands will apply to C:"

end sub



sub dsklist

for a=1 to 16
if DSKFILE(a) <> "" then
        print chr$(a+64);": ";DSKFILE(a)
endif
next a

end sub

sub DSKSELECT

        rem Sanitize drive selector.
        a=asc(left$(comm,1))
        if a>96 then a=a-32
        if a<65 then goto DSKERROR
        if a>80 then goto DSKERROR
        a=a-64

        if DSKFILE(a)="" then goto DSKERROR2
        defaultdrive=a
        DISK=LoadFile(DSKFILE(a))
        drive=a-1

        Print "Opening ";chr(a+64)+": as default drive"

        CrossWarning=1

        init (DISK)                                                     : rem Initialise the expected disk and read it's data.

        BuildDirectory (DISK)                           : rem Check the disk we just initialised.

        CrossWarning=0


        goto DSKEXIT
        DSKERROR:
        print "Invalid Drive Selection." : goto DSKEXIT : rem make no change.
        DSKERROR2:
        print "No image mapped to drive requested.(2)" : goto DSKEXIT
DSKEXIT:
rem Done here.
end sub


sub tempswitch                                                                  : rem Temporarily switch disk selected if specified for this command.
        sfile=argument(CLI,2)                                           : rem temp store of the current filename in question if it exists.
        loggeddrive=drive                                               : rem save the presently logged file.
        dspec=""

        if mid(sfile,2,1)=":" then                                      : rem Do we have a situation with a drive specifier?
                dspec=left(sfile,2)
                sfile=right(sfile,len(sfile)-2)                 : rem remove the extra.
                loggeddrive=drive
                comm=dspec
                DSKSELECT                                       : rem Temporarily change to specified drive and initialise.
        endif
end sub

sub tempreturn                                                                  : rem Return to the logged drive after a different disk operation.
        if drive <> loggeddrive then drive=loggeddrive  : INITIALISE                            : rem Clean up logged drive.                                                                            : avoid triggering anything else.
        comm="Null"
end sub



sub Showdir

dim displayed as integer
displayed=0

SERROR=0        : rem Prepare for any sub error detection.

        sfile=GetDrive(argument(CLI,2))                                                                                                                         : rem Let's load the correct image, clean up the filename we want to delete and start building the extent.
        if SERROR>0 then goto SHOWDIREX         : rem Error out.

                if sfile=chr(13) then sfile=""

                        rem print "Searching for files;"

                for a=1 to numfiles
                        if len(SFILE)<>0 then                                                                                                           : rem Was an argument supplied?
                                if      matchfilename(sfile,mid(files(a),2,11)) =1 then                                                 : rem If so, did it match the filenames picked out?
                                        print stripbit7(mid$(files(a),2,8));".";stripbit7(mid$(files(a),10,3));"   ";filesizes(a);" Bytes"
                                        displayed=displayed+1
                                endif
                        else
                                print stripbit7(mid$(files(a),2,8));".";stripbit7(mid$(files(a),10,3));"   ";filesizes(a);" Bytes"              : rem If no argument supplied, then just show everything.
                        endif
                next a

                if displayed>0 then print "Showing";displayed;" files out of";
                print numfiles;" Files on disk image"

                Print " Disk capacity:";(allocs-AMDirectory)*AMActualBS; " bytes"

                print FREESPACE;" bytes free in ";FREESPACE/AMActualBS;" allocations."
SHOWDIREX:

end sub

sub delfile
        rem Just call delete function now with second argument in CLI

        if argument(CLI,2) <> "" and argument(CLI,2) <> chr(13) then
                deletefile(Argument(CLI,2))
rem     print "Default Drive:";defaultdrive
rem     print "Default File:";defaultfname
        else
                print "Error: No filename specified."
        endif

end sub

sub exfile
rem Examine File.

        sfile=getdrive(argument(CLI,2))

        dim localloop as integer


        for localloop=1 to numfiles
                if      matchfilename(sfile,mid(files(localloop),2,11)) =1 then                                                 : rem If so, did it match the filenames picked out?
                        FILE=FileAttributes(mid(files(localloop),2,11))                                                                                : rem And now we get the exact filename...
						rem Nothing really comes back. We just show the file details -eg, allocations, extents etc. 
                endif
        next localloop
end sub



sub Typefile                                                                                            : rem Display a file in hex. Might want to ASCII it later.
        sfile=getdrive(argument(CLI,2))

        dim localloop as integer


        for localloop=1 to numfiles
                if      matchfilename(sfile,mid(files(localloop),2,11)) =1 then                                                 : rem If so, did it match the filenames picked out?
                        FILE=GetFile(mid(files(localloop),2,11))                                                                                : rem And now we get the exact filename...

                        print FILE

                        print "File Length:";len(FILE)
                endif
        next localloop

end sub


sub local2cpm                                                                                                           : rem copy a file from the local directory to a CPM directory.

dim lfilenum as integer : rem number of local files.
dim lfiles as string    : rem Output of directory command ( with cr/lf intact).
dim localloop as integer : rem local loop
dim sendcom as string   : rem command to send to the shell.

        sendcom="dir /a-d /b "+argument (cli,2)+ " > localdirectory.txt"

        shell sendcom

lfiles=loadfile ("localdirectory.txt")
lfiles=justtext(lfiles)         : rem Strip any control characters, convert them to spaces.

print "We found:"
print lfiles
print "End of files. "

lfilenum=1
local2cpm1:

defaultfname=argument(lfiles,lfilenum)
lfilenum=lfilenum+1

if defaultfname=chr(13) or defaultfname=chr(10) then goto local2cpm1    : rem Not a filename, not EOF.
if defaultfname="" then goto local2cpmex


rem     sfile=getdrive(argument(CLI,2))                                                                 : rem temp store of the current filename in question if it exists.  Already cleaned. And Initialize the correct drive.


                FILE=loadfile(defaultfname)
                        defaultdsk=""                                                                           : rem Not relevant here.
                                Print "Copying:";defaultfname
rem                                DISK=PutFile(DISK,FILE,argument(CLI,3))                                                                                 : rem NOW WRITE THE FILE we just read.
                                DISK=PutFile(DISK,FILE,defaultfname)                                                                                 : rem NOW WRITE THE FILE we just read.

goto local2cpm1

local2cpmex:

end sub

sub cpm2local                                                                                                           : rem copy a file from the local directory to a CPM directory.

sfile=getdrive(argument(CLI,2))                                                                 : rem temp store of the current filename in question if it exists.  Already cleaned. And Initialize the correct drive.

        dim localloop as integer

        for localloop=1 to numfiles
                        GetDrive(argument(CLI,2))                                                                                                                                               : rem Let's load the correct image, clean up the filename and start building the extent.
                        if      matchfilename(sfile,mid(files(localloop),2,11)) =1 then                                                 : rem If so, did it match the filenames picked out?
                                FILE=GetFile(mid(files(localloop),2,11))                                                                                : rem And now we get the exact filename...
                                defaultfname=mid(files(localloop),2,11)                                                                                 : rem Remember the current matched file name in case none is specified next.
                                Print "Copying:";defaultfname

                                FILE=SaveAsFile(FILE,defaultfname)                                                                      : rem Now write to the local drive...

                        endif
        next localloop

end sub

sub copyfile                                                                                            : rem Display a file in hex. Might want to ASCII it later.

        sfile=getdrive(argument(CLI,2))                                                                 : rem temp store of the current filename in question if it exists.  Already cleaned. And Initialize the correct drive.

        dim localloop as integer

        for localloop=1 to numfiles
                        GetDrive(argument(CLI,2))                                                                                                                                               : rem Let's load the correct image, clean up the filename and start building the extent.
                        if      matchfilename(sfile,mid(files(localloop),2,11)) =1 then                                                 : rem If so, did it match the filenames picked out?
                                FILE=GetFile(mid(files(localloop),2,11))                                                                                : rem And now we get the exact filename...
                                defaultfname=mid(files(localloop),2,11)                                                                                 : rem Remember the current matched file name in case none is specified next.
                                Print "Copying:";defaultfname
                                DISK=PutFile(DISK,FILE,argument(CLI,3))                                                                                 : rem NOW WRITE THE FILE we just read.
                        endif
        next localloop

end sub

sub Textfile                                                                                            : rem Display a file in hex. Might want to ASCII it later.
        sfile=getdrive(argument(CLI,2))                                         : rem temp store of the current filename in question if it exists.

        dim localloop as integer

        rem DEBUG print "Checking Text:";dspec;":";driveok(dspec)

                for localloop=1 to numfiles
                                if      matchfilename(sfile,mid(files(localloop),2,11)) =1 then                                                 : rem If so, did it match the filenames picked out?
                                        FILE=GetFile(mid(files(localloop),2,11))                                                                                : rem And now we get the exact filename...

                                        for a=1 to len (FILE)
                                                if asc(mid(FILE,a,1)) > 31 and asc(mid(FILE,a,1)) < 127 then
                                                        print chr(asc(mid(FILE,a,1)) mod 128);
                                                endif
                                                if asc(mid(FILE,a,1)) = 13 then print
                                        next a
                                endif
                next localloop

end sub


sub Dumpfile                                                                                            : rem Display a file in hex. Might want to ASCII it later.
        FILE=GetFile(CaseFix(Argument(CLI,2)))
        PRINTHEX(file,len(file))
end sub

sub hexdib

        printhex(mid(DISK,1,256),256)

end sub

sub hextib

        printhex(mid(DISK,257,256),256)

end sub

sub HexDumpAllocation                                                                           : rem Dump out an allocation as numbered in the CLI.
        BLOCK=getallocation(DISK,val(argument(CLI,2)))
        print
        print "Len: ";len(BLOCK)
        PRINTHEX (BLOCK,len(BLOCK))
end sub

sub HexDumpSector                                                                                       : rem Display any sector. From the start. Useful for examining the disk if there's corruption.
rem     BLOCK=getlogicalsector(DISK,val(argument(CLI,2)))
        BLOCK=getsect(DISK,val(argument(CLI,2)))
        print
        print "Len: ";len(BLOCK)
        PRINTHEX (BLOCK,len(BLOCK))
end sub

sub HexDumpLogical                                                                                      : rem Display any LOGICAL sector. From the start. Useful for examining the disk if there's corruption.
rem     BLOCK=getlogicalsector(DISK,val(argument(CLI,2)))
        print "Routine currently disabled. "
rem         print "Len: ";len(BLOCK)
rem        PRINTHEX (BLOCK,len(BLOCK))
end sub

sub HexDumpBoot                                                                                         : rem Hex dump of the whole boot sector, and the disk information block.
        PRINTHEX (mid$(DISK,1,4096),4096)
end sub



sub HexDumpTrack                                                                                        : rem Hex dump of the whole boot sector, and the disk information block.
        BLOCK=getlogicaltrack(DISK,val(argument(CLI,2)))
        print
        print "Len: ";len(BLOCK)
        PRINTHEX (BLOCK,len(BLOCK))
end sub


sub hextrack                                                                                            : rem Hex dump of an entire track..... Useful for checking Track0.
        Printhex (getlogicaltrack(DISK,val(argument(CLI,2))),len(getlogicaltrack(DISK,val(argument(CLI,2)))))
        rem     Printhex (getlogicaltrack(DISK,val(argument(CLI,2))),tracklen)
end sub

sub Verbose

debug=val(argument(CLI,2))

end sub



sub default9                    : rem If there's no data written into the file, some defaults can be applied to make the disk barely readable.

                                Print "Applying a default config ( only use if the disk image is corrupted, or missing config data. )"

                        TRACKS=40
                        SIDES=1
                        TRACKLEN=4864
                        Sectorsize=2
                        Numsectors=9
                        AMType=99
                        AMSides=0
                        AMTracks=40
                        AMSectors=9
                        AMSectSize=2
                        AMActualSS=512
                        AMBlockSize=3
                        AMActualBS=1024
                        AMDirectory=2
                        AMReserved=1
                                allocs=int((numsectors*(tracks-1)-AMReserved)/(AMActualBS/AMActualSS))*sides
								SectorsPerAllocation=AMActualBS/AMActualSS

                                DIRECTORY       :rem Regenerate Directory ( If Available )

end sub

sub defaultx                    : rem If there's no data written into the file, some defaults can be applied to make the disk barely readable.

                                Print "Applying a default config ( only use if the disk image is corrupted, or missing config data. ) Type: Amstrad No Reserved Tracks. 2 Allocations."

                        TRACKS=40
                        SIDES=1
                        TRACKLEN=4864
                        Sectorsize=2
                        Numsectors=9
                        AMType=99
                        AMSides=0
                        AMTracks=40
                        AMSectors=9
                        AMSectSize=2
                        AMActualSS=512
                        AMBlockSize=3
                        AMActualBS=1024
                        AMDirectory=2
                        AMReserved=0
                                allocs=int((numsectors*(tracks-1)-AMReserved)/(AMActualBS/AMActualSS))*sides
								SectorsPerAllocation=AMActualBS/AMActualSS

                                DIRECTORY       :rem Regenerate Directory ( If Available )

end sub

sub default10

                                Print "Applying a default config ( only use if the disk image is corrupted, or missing config data. )"

                        TRACKS=40
                        SIDES=1
                        TRACKLEN=5120
                        Sectorsize=2
                        Numsectors=10
                        AMType=99
                        AMSides=0
                        AMTracks=40
                        AMSectors=10
                        AMSectSize=2
                        AMActualSS=512
                        AMBlockSize=3
                        AMActualBS=1024
                        AMDirectory=2
                        AMReserved=1
                                allocs=int((numsectors*(tracks-1)-AMReserved)/(AMActualBS/AMActualSS))*sides
								SectorsPerAllocation=AMActualBS/AMActualSS

                                DIRECTORY       :rem Regenerate Directory ( If Available )

end sub



sub match
        print matchfile(Argument(CLI,2),0)
end sub


sub showhistory
        for a=1 to archaic
        print history(a)
        next a
end sub


sub format

        rem Sanitize drive selector.
        if mid$(Argument(CLI,2),2,1) <> ":" then goto DSKERROR
        a=asc(mid(Argument(CLI,2),1,1))
        if a>96 then a=a-32
        if a<65 then goto DSKERROR
        if a>80 then goto DSKERROR
        a=a-64

ftracks=parameter("TRACKS",40)
fheads=parameter("HEADS",1)-1 : rem 0 value = 1 head.
fsectors=parameter("SECTORS",9)
fsectsize=parameter("SECTSIZE",512)

print "Formatting with following Parameters"
print "Tracks:";ftracks,"Heads:";fheads+1,"Sectors per track:";fsectors,"Sector size:";fsectsize
print "If incorrect, specify, eg: FORMAT A: NEW.DSK TRACKS=40 HEADS=1 SECTORS=9 SECTSIZE=512"


        if dskfile(a)<>"" then
                print "WARNING. Selected drive is mounted - DSK FILE [";dskfile(a);"] will be erased."
        else
                print "Selected drive not mounted. Will create a new DSK file and mount it."
        endif

        print "Format Drive ";chr(a+64);" - Proceed? (Y/N)"

        formatcheck:
        k=inkey
        if k<>"y" and k<>"Y" and k<>"n" and k<>"N" then goto formatcheck
        if k<>"y" and k<>"Y" then goto DSKERROR2


disk=adddib ("")

for ftracknum=0 to ftracks-1
for fsidenum=0 to fheads

disk=disk+addtib ("")
disk=disk+addtrack ("")

next fsidenum
next ftracknum

disk=addamstrad (disk)  : rem Add amstrad disk identified to file. ( Boot sector ).

if argument(CLI,3)<>"" then savefile(argument(CLI,3))

if argument(CLI,3)<>"" then dskfile(a)=argument(CLI,3)


        goto DSKEXIT
        DSKERROR:
        print "Invalid Drive Selection." : goto DSKEXIT : rem make no change.
        DSKERROR2:
        print "Format aborted" : goto DSKEXIT
DSKEXIT:
rem Done here.
end sub






sub SliceFile

LiteralHex ( DISK, val(Argument(CLI,2))+1 , val(Argument(CLI,3)) + val(Argument(CLI,2)) )

end sub


sub localdir

shell "dir "+argument(CLI,2)

end sub



sub LocalDirPath

dim path as string
LDIR=argument(CLI,2)

print
print "Currently selected directory contents:"

path="dir "+LDIR

shell path

end sub






sub localcd

shell "cd "+argument(CLI,2)

end sub




sub BootCopy
rem Copy a boot sector from one disk to another. Experimental.

dim sdata as string
dim XDPB as string                                                              : rem DONT copy the XDPB with the sector.

        SERROR=0

        GetDrive(argument(CLI,2))                                       : rem Get the correct source disk if presented.
        sdata=getsect(DISK,1)                                           : rem Get the Boot Sector.

        GetDrive(argument(CLI,3))                                       : rem Get the correct destination disk if presented.
        XDPB=left(getsect(DISK,1),15)+chr(0)                            : rem Save the new XDPB and zero out the Fiddle Byte.

        sdata=insert(sdata,XDPB,1)                                      : rem Splice in the new XDPB to the old boot sector.

        DISK=PutSect (DISK, sdata, 1)                                   : rem Write the boot sector.



        FIDDLE(DISK,val(argument(CLI,4)))                               : rem Create the fiddle byte.

        if argument(CLI,4)="" then SERROR=SERROR+1                      : rem Check we had 3 delimiters in the command as a basic check. Trust them though.

        sdata=getsect(DISK,1)                                           : rem reload the sector

        if SERROR<>0 then
                Print "Error - Bad Syntax - Example correct is  'bcopy a: b: 3' to copy the boot from a: to b: and set the checksum fiddle to produce 3. "
                Print "- To change a specific drive, eg. A: without copying, use 'bcopy a: a: 3'"
                Print "- Aborting current operation."
        endif

        if SERROR<>0 then goto BOOTCOPYEND

        Print "Destination Drive Check."
        printhex (sdata,512)
        print "Checksum:";hex(getchecksum(sdata))
        print "BCheck:";hex(  asc(  mid(sdata,16,1) ) )
        print

        savefile(DEFAULTDSK)

BOOTCOPYEND:

end sub



sub sectorcopy


end sub

sub allocopy

end sub



sub Mount
rem Mounts a drive into the drive list.

dim drivenumber as integer

        if fileexists(argument(CLI,2))=0 then

        drivenumber=asc(left$(argument(CLI,3),1))-64            : rem adjust for letter to number.
        if mid$(argument(CLI,3),2,1) <> ":" then drivenumber=99 : print "Drive allocator incorrect- Try something like 'mount FILENAME.EXT B:' as an example"
        if drivenumber>16 then drivenumber=drivenumber-32       : rem Correct for case.
                if drivenumber > 0 and drivenumber < 17 then
                        print "Mounting ";argument(CLI,2);" as ";argument(CLI,3)
                        DSKFILE(drivenumber)=argument(CLI,2)            : rem Mount the drive image
                else
                Print " Drive '";argument(CLI,3);"' out of bounds. From A: to P: only"
                print " Try something like 'mount disk1.dsk a:'"
                endif
        else
        Print "Image file '";argument(CLI,2);"' does not exist. Cannot mount."
        print " Try something like 'mount disk1.dsk a:'"
        endif
end sub


sub UnMount
rem Mounts a drive into the drive list.

dim drivenumber as integer

        drivenumber=asc(left$(argument(CLI,2),1))-64            : rem adjust for letter to number.
        if mid$(argument(CLI,2),2,1) <> ":" then drivenumber=99 : print "Drive allocator incorrect- Try something like 'mount FILENAME.EXT B:' as an example"
        if drivenumber>16 then drivenumber=drivenumber-32       : rem Correct for case.

        if drivenumber > 0 and drivenumber < 17 then
                print "Unmounting ";argument(CLI,2)
                        DSKFILE(drivenumber)=""                 : rem Mount the drive image
        else
                Print "Drive number ";drivenumber;" out of bounds. From A: to P: only"
        endif
end sub


sub SAVECONFIG
rem Let's save the configuration ( eg, mounted files ) to make it persistent.

open "cpm.cfg" for output as #1

print #1,"# Configuration file - Can be saved by typing save into console while running application. "


for a=1 to 16
if DSKFILE(a) <> "" then
        print #1,"mount ";chr$(34);DSKFILE(a);chr$(34);" ";chr$(a+64);": "
endif
next a

rem Save selected drive.
print #1,chr$(drive+65);":"


close #1


end sub

	
sub SHOWINTERLEAVE			
rem Show the Interleave patten in the DSK file...

	for a= 1 to AMTracks
		print "Track:";a;"   ";
	for b=1 to AMSectors
	
		print translate(b,a);":";
	
	next b
		print
	next a

end sub


sub TESTCOMMANDS
rem Check if the input line can be processed as a command.

comm=Argument(CLI,1)

if comm="" then comm="null"
if comm=chr(13) then comm="null"

if casefix(comm)="HELP" then help : comm="null"
if comm="?" then help : comm="null"
rem if comm="#" then comm="null" : rem We will treat it as a comment eg, # This is a comment line.
if casefix(comm)="DIR" then showdir : comm="null"
if casefix(comm)="STAT" then stat : comm="null"
if casefix(comm)="FAT" then showfat : comm="null"
if casefix(comm)="DIRNUM" then dirnum : comm="null"
if casefix(comm)="DIRHEX" then hexdir : comm="null"
if casefix(comm)="LIST" then dsklist : comm="null"
if casefix(comm)="TYPE" then typefile : comm="null"
if casefix(comm)="TEXT" then textfile : comm="null"
if casefix(comm)="COPY" then copyfile : comm="null"
if casefix(comm)="BCOPY" then bootcopy : comm="null"
if casefix(comm)="LCOPY" then local2cpm : comm="null"
if casefix(comm)="LPUT" then cpm2local : comm="null"
if casefix(comm)="SCOPY" then sectorcopy : comm="null"
if casefix(comm)="ACOPY" then allocopy : comm="null"
if casefix(comm)="LDIR" then localdir : comm="null"
if casefix(comm)="LCD" then localcd : comm="null"
if casefix(comm)="DUMP" then dumpfile : comm="null"
if casefix(comm)="ALLOCATION" then hexdumpallocation : comm="null"
if casefix(comm)="SECTOR" then hexdumpsector : comm="null"
if casefix(comm)="LOGICAL" then hexdumplogical : comm="null"
if casefix(comm)="BOOT" then hexdumpboot : comm="null"
if casefix(comm)="TRACK" then hexdumptrack : comm="null"
if casefix(comm)="DEFAULT9" then default9 : comm="null"
if casefix(comm)="DEFAULTX" then defaultx : comm="null"
if casefix(comm)="DEFAULT10" then default10 : comm="null"
if casefix(comm)="HEXTRACK" then hextrack : comm="null"
if casefix(comm)="MATCH" then match : comm="null"
if casefix(comm)="DELETE" then delfile : comm="null"
if casefix(comm)="EXAMINE" then exfile : comm="null"
if casefix(comm)="DEL" then delfile : comm="null"
if casefix(comm)="CLS" then cls: comm="null"
if casefix(comm)="TEST" then testroutine : comm="null"
if casefix(comm)="DEBUG" then verbose : comm="null"
if casefix(comm)="FORMAT" then format : comm="null"
rem if casefix(comm)="COPYBOOT" then copyboot : comm="null"
if casefix(comm)="SLICE" then slicefile : comm ="null"
if casefix(comm)="TIB" then hextib : comm="null"
if casefix(comm)="DIB" then hexdib : comm="null"
if casefix(comm)="PATH" then LocalDirPath : comm="null"


if casefix(comm)="INTERLEAVE" then SHOWINTERLEAVE : comm="null"

if casefix(comm)="RENAME" then rename : comm="null"
if casefix(comm)="RENAMENUM" then renamenum : comm="null"

if casefix(comm)="MOUNT" then mount : comm="null"
if casefix(comm)="UNMOUNT" then unmount : comm="null"

rem TAPE functions.
if casefix(comm)="TAPECOPY" then TAPECOPY : comm="null"
if casefix(comm)="TAPEDIR" then TAPEDIR : comm="null"
if casefix(comm)="ZXLIST" then ZXLIST : comm="null"

rem stuff I need here.
rem FORMAT a DISK.
rem COPY a DISK including the boot sectors ( reserved sectors )
rem CHANGE a disk format... so I can change from 180K to 720K, and copy the functionality....
rem LOCAL disk operations ( eg, Dir, Move files from DOS to CP/M etc ) - Maybe have virtual directories and unpack the files there?
rem Microdrive routines back into this program.
rem Do I need a GUI? I hate guis.


if casefix(comm)="HISTORY" then showhistory : comm="null"               : rem show CLI historydum.

if len(comm)=2 and right$(comm,1)=":" then DSKSELECT    : comm="null"   : rem Disk selection routine.

end sub


rem ###################################################################################################
rem
rem MAIN ROUTINES START HERE>
rem
rem ###################################################################################################





main:
rem Some code probably should go here.
rem

UPPERZXCHAR     : rem Load in ZX character set.
                : rem for when printing out ZX Tokenised Basic characters.

debug=0                                                                         : rem Set Debug Output to OFF>

rem print "CP/M Disk Image Tool, Version 0.1 ALPHA."
rem print "Type ? or HELP and press enter to see list of commands. Q, EXIT or QUIT or Crtl-C to return to DOS"
rem print

rem CONFIG=LoadFile("cpm.cfg")
drive=-2 : rem -2 does not exist. Means no drive selected. If no drives are loaded, then make the prompt a ?

SInterleaved=0  : rem By default, not interleaved.

defaultresource="A:"
defaultdrive=1
defaultfname=""                         : rem No current default filename. Might make this default to *.* later. Not sure.

if DSKFILE(1) <> "" then DISK=LoadFile(DSKFILE(1)) : drive=0 : print "Opening ";DSKFILE(1);" as A>"

rem setup variables.
rem DISK=LoadFile("disk.dsk")                           : rem LOAD UP THE DISK INTO A STRING VARIABLE.

CrossWarning=1  : rem Note Crosslinked files in images.
        rem INITIALISE

        INIT (DISK)
        BuildDirectory (DISK)

        rem STAT
CrossWarning=0

viewalloc=0


rem Let's process any incoming batch file here...
rem eg. Load in files to mount from config
rem First read in any files to mount. Only store this information in CPM.CFG
open "cpm.cfg" for input as #1
READBATCH:
        INPUT #1,CLI
        CLI=CLI+" " : rem padding. s
        if left$(CLI,1)="#" then goto READBATCH
        print chr(13);CHR$(drive+65);">";CLI
        TESTCOMMANDS            : rem Process the commands in the batch file as though typed.
        print                   : rem print a spare line between commands and remove any lack of EOL.
        if CLI<>" " then goto READBATCH
close #1

rem Let's process any incoming batch file here... Part 2.
rem eg. Load in files to mount from config
rem First read in any files to mount. Only store this information in CPM.CFG
open "cpm.bat" for input as #1
READBATCH2:
        INPUT #1,CLI
        CLI=CLI+" " : rem padding. s
        if left$(CLI,1)="#" then goto READBATCH2
        print chr(13);CHR$(drive+65);">";CLI
        TESTCOMMANDS            : rem Process the commands in the batch file as though typed.
        print                   : rem print a spare line between commands and remove any lack of EOL.
        if CLI<>" " then goto READBATCH2
close #1

rem We separate the above routines, so we can write the "mounted" drives and similar to CPM.CFG without obliterating any BAT file.


print "CP/M Disk Image Tool, Version 0.1 ALPHA."
print "Type ? or HELP and press enter to see list of commands. Q, EXIT or QUIT or Crtl-C to return to DOS"
print



CLI=""


print CHR$(drive+65);">";

PRE0CLILOOP:
CLI=""
POSTCLI=""
walk=1  : rem - Show no history. rem 1 = current line. 2 = first history.


PRECLILOOP:
print chr(13);CHR$(drive+65);">";CLI;                                           : rem Don't show POSTCLI here. It might already be there.


CLILOOP:
KEY=INKEY
if KEY="" then goto CLILOOP


if KEY <> chr(8) and KEY <> chr$(13) and len(KEY)=1 then                        : rem Don't go printing Backspaces or CRs here. Or add them.
        CLI=CLI+KEY
rem     print KEY;
        print chr(13);CHR(drive+65);">";space(len(CLI));space(len(POSTCLI));
        print chr(13);CHR$(drive+65);">";CLI;POSTCLI;
        endif

if KEY=chr(8) then                                                              : rem Here's how we deal with a backspace.
        print chr(13);CHR(drive+65);">";space(len(CLI));space(len(POSTCLI));
        if len(CLI)>0 then CLI=left(CLI,len(CLI)-1)
        print chr(13);CHR$(drive+65);">";CLI;POSTCLI;
        endif

if left$(KEY,1)=chr(255) then
        key=right$(key,1)                                                                       : rem Escape codes ( 255 then this code ).

        if key=chr(75) and len(CLI)>0 then
                        rem LEFT
                        POSTCLI=right(CLI,1)+POSTCLI
                        CLI=LEFT(CLI,len(CLI)-1)                                                : rem Shuffle characters into POST CLI.
                endif

        if key=chr(77) then
                        rem RIGHT
                        if len(POSTCLI)>0 then                                                  : rem Check to see if we can go right...
                                CLI=CLI+left(POSTCLI,1)
                                POSTCLI=right(POSTCLI,len(POSTCLI)-1)
                        endif
                endif

        if key=chr(72) then
                        rem Up
                        print chr(13);CHR(drive+65);">";space(len(CLI));space(len(POSTCLI));    : rem Erase current line.

                        history(walk)=CLI+POSTCLI                                               : rem Store current line including changes.
                        if walk < archaic then walk = walk + 1                                  : rem Update place in history.
                        CLI=history(walk)                                                       : rem And switch in historical CLI.

                        if right(CLI,1)=chr(13) then CLI=left$(CLI,len(cli)-1)                  : rem Remove any EOL from history.
                        POSTCLI=""                                                              : rem And we're going to start at the end of the line.
                endif

        if key=chr(80) then
                        rem Down
                        print chr(13);CHR(drive+65);">";space(len(CLI));space(len(POSTCLI));    : rem Erase current line.

                        history(walk)=CLI+POSTCLI                                               : rem Store current line including changes.
                        if walk > 0 then walk = walk - 1                                        : rem Update place in history.
                        CLI=history(walk)                                                       : rem And switch in historical CLI.
                        if walk=0 then CLI="" : walk = 1                                        : rem Future case - Erase current edited line.
                                                                                                : rem WALK=2 past. WALK=1 current. Walk=0 future.
                        POSTCLI=""
                        if right(CLI,1)=chr(13) then CLI=left$(CLI,len(cli)-1)
                endif

        if key=chr(83) then
                        rem Delete
                        if len(POSTCLI)>0 then                                                  : rem Check to see if we can go right...
                                print chr(13);CHR(drive+65);">";space(len(CLI));                : rem Erase current line to cursor only.
                                POSTCLI=right(POSTCLI,len(POSTCLI)-1)                           : rem Delete characters to the right.
                                print POSTCLI;" ";                                              : rem Print POSTCLI and erase last character.
                        endif
                endif

endif


if KEY <> chr$(13) then goto PRECLILOOP

CLI=CLI+POSTCLI+chr(13)                                                                         : rem Make these two one now. It's executed.Add CR

for a=archaic to 3 step -1
history(a)=history(a-1)
next a
walk=1                                                                                          : rem Set history pointer back to current.
history(walk+1)=CLI                                                                             : rem Present line becomes history.
history(walk)=""                                                                                : rem Clear the present.

print : rem We need a new line enforced here as we are at the end of the command.
rem debug for a=1 to len(CLI) : print hex(asc(mid(CLI,a,1)));" ";: next a : print



rem print: print CLI
rem print "1:";Argument(CLI,1);":";len(Argument(CLI,1))
rem print "2:";Argument(CLI,2);":";len(Argument(CLI,2))
rem print "3:";Argument(CLI,3);":";len(Argument(CLI,3))
rem print "4:";Argument(CLI,4);":";len(Argument(CLI,4));":";asc(Argument(CLI,4))

TESTCOMMANDS    : rem Check if we have a command.

if casefix(comm)="SAVE" then SAVECONFIG : comm="null"

if casefix(comm)="QUIT" then goto endit
if casefix(comm)="EXIT" then goto endit
if casefix(comm)="Q" then goto endit

if len(comm)=2 and right$(comm,1)=":" then DSKSELECT    : comm="null"

if comm <> "null" then print "Error: Command not understood. Type help for help."

print
print CHR$(drive+65);">";
CLI=""

goto CLILOOP


endit:

end



DATA "RND","INKEY$","PI","FN","POINT","SCREEN$","ATTR","AT","TAB","VAL$","CODE"
DATA "VAL","LEN","SIN","COS","TAN","ASN","ACS","ATN","LN","EXP","INT","SQR","SGN","ABS","PEEK","IN"
DATA "USR","STR$","CHR$","NOT","BIN","OR","AND","<=",">=","<>","LINE","THEN","TO","STEP","DEF FN","CAT"
DATA "FORMAT","MOVE","ERASE","OPEN #","CLOSE #","MERGE","VERIFY","BEEP","CIRCLE","INK","PAPER","FLASH","BRIGHT","INVERSE","OVER","OUT"
DATA "LPRINT","LLIST","STOP","READ","DATA","RESTORE","NEW","BORDER","CONTINUE","DIM","REM","FOR","GO TO","GO SUB","INPUT","LOAD"
DATA "LIST","LET","PAUSE","NEXT","POKE","PRINT","PLOT","RUN","SAVE","RANDOMIZE","IF","CLS","DRAW","CLEAR","RETURN","COPY"



