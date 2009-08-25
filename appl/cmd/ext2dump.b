implement Ext2dump;

include "sys.m";
	sys: Sys;
	print, sprint: import sys;
include "draw.m";
include "arg.m";
include "../lib/ext2.m";
	ext2: Ext2;
	Super, Group, Inode, Entry, Part: import ext2;

Ext2dump: module {
	init:	fn(nil: ref Draw->Context, args: list of string);
};


dflag: int;
part: ref Part;

init(nil: ref Draw->Context, args: list of string)
{
	sys = load Sys Sys->PATH;
	arg := load Arg Arg->PATH;
	ext2 = load Ext2 Ext2->PATH;
	ext2->init();

	arg->init(args);
	arg->setusage(arg->progname()+" [-d] file");
	while((c := arg->opt()) != 0)
		case c {
		'd' =>	dflag++;
		* =>	arg->usage();
		}
	args = arg->argv();
	if(len args != 1)
		arg->usage();
	file := hd args;
	fd := sys->open(file, Sys->OREAD);
	if(fd == nil)
		fail(sprint("open %q: %r", file));

	err: string;
	(part, err) = Part.init(fd);
	if(err != nil)
		fail(err);

	print("%s", part.s.text());
	print("%s", part.group.text());
	print("%s", part.root.text());

	dumpdir(part.root);
}

dumpdir(i: ref Inode)
{
	if((i.mode & ext2->IMdir) == 0)
		fail(sprint("not a directory"));

	end := (i.size+big part.bsize-big 1)/big part.bsize;
	for(bn := big 0; bn < end; bn++) {
		err := part.getblock(i, buf := array[part.bsize] of byte, bn);
		if(err != nil)
			fail(err);
		eo := 0;
		while(eo+ext2->Entryminlen <= len buf) {
			e: ref Entry;
			(e, err) = Entry.parse(buf[eo:], part.s.featincompat&ext2->IncompatFiletype);
			if(err != nil)
				fail("parsing entry: "+err);
			eo += e.elen;
			if(e.inode == 0) {
				say("unused entry");
				continue;
			}
			print("%s\n", e.text());
			ci: ref Inode;
			(ci, err) = part.inodeget(e.inode);
			if(err != nil)
				fail("reading file's inode: "+err);
			print("%s\n", ci.text());
		}
	}
}

warn(s: string)
{
	sys->fprint(sys->fildes(2), "%s\n", s);
}

say(s: string)
{
	if(dflag)
		warn(s);
}

fail(s: string)
{
	warn(s);
	raise "fail:"+s;
}
