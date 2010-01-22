Ext2: module
{
	PATH:	con "/dis/lib/ext2.dis";

	dflag:	int;
	init:	fn();

	Magic:	con 16rEF53;

	Superlen:	con 1024;
	SScleanunmount,
	SSunclean:	con 1+iota;
	SEcontinue,
	SEreadonly,
	SEpanic:	con 1+iota;
	SRgoodold,
	SRdynamic:	con iota;

	Grouplen:	con 32;

	IMsocket:	con 12<<12;
	IMsymlink:	con 10<<12;
	IMregular:	con 8<<12;
	IMblockdev:	con 6<<12;
	IMdir:		con 4<<12;
	IMchardev:	con 2<<12;
	IMfifo:		con 1<<12;
	IMfilemask:	con 15<<12;

	IMsetuid:	con 8<<8;
	IMsetgid:	con 4<<8;
	IMsticky:	con 2<<8;
	IMspecialmask:	con 2r1110<<8;
	IMmodemask:	con 8r777;

	IFsecrm,
	IFunrm,
	IFcompr,
	IFsync,
	IFimmuntable,
	IFappend,
	IFnodump,
	IFnoatime,
	IFdirty,
	IFncomprblocks,
	IFnocompr,
	IFecompr:	con 1<<iota;
	IFbtree,
	IFimagic,
	IFjournaldata:	con 1<<16;
	IFreserved:	con 1<<31;

	IFTunknown,
	IFTregular,
	IFTdir,
	IFTchardev,
	IFTblockdev,
	IFTfifo,
	IFTsocket,
	IFTsymlink:	con iota;

	Entryminlen:	con 8;

	CompatDirprealloc,
	CompatImagicinodes,
	CompatHasjournal,
	CompatExtattr,
	CompatResizeino,
	CompatDirindex:	con 1<<iota;

	IncompatCompression,
	IncompatFiletype,
	IncompatRecover,
	IncompatJournaldev,
	IncompatMetabg:	con 1<<iota;

	ROCompatSparsesuper,
	ROCompatLargefile,
	ROCompatBtreedir:	con 1<<iota;

	Super: adt {
		ninodes,
		nblocks,
		nrblocks,
		nfreeblocks,
		nfreeinodes,
		firstdblock:	big;
		blocklog,
		fraglog:	int;
		ngroupblocks,
		ngroupfrags,
		ngroupinodes:	int;
		mtime,
		wtime,
		nmnt,
		maxmnt,
		magic,
		state,
		errors,
		minorrev,
		lastcheck:	int;
		checkival,
		creator,
		rev:		int;
		defresuid,
		defresgid:	int;

		# for rev == dynamic
		firstinode,
		inodesize,
		blockgroup:	int;
		featcompat,
		featincompat,
		featrocompat:	int;
		uuid,
		volumename:	array of byte;
		lastmounted:	array of byte;
		bitmapalg:	int;

		preallocblocks,
		preallocdirblocks:	int;

		journaluuid:	array of byte;
		journalinum,
		journaldev,
		lastorphan:	int;

		hashseed:	array of int; # 4 ints
		hashversion:	int;

		defmntopts:	int;
		firstmetabg:	int;

		parse:	fn(buf: array of byte): (ref Super, string);
		read:	fn(fd: ref Sys->FD, off: big): (ref Super, string);
		text:	fn(s: self ref Super): string;
	};
	featcompatstr,
	featincompatstr,
	featrocompatstr:	fn(v: int): string;

	Group: adt {
		blockbitmap,
		inodebitmap,
		inodetab:	big;
		nfreeblocks,
		nfreeinodes,
		nuseddirs:	int;
		reserved:	array of byte;

		read:	fn(fd: ref Sys->FD, off: big): (ref Group, string);
		parse:	fn(buf: array of byte): (ref Group, string);
		text:	fn(g: self ref Group): string;
	};

	Syminodesizemax:	con 60-1;
	Inode: adt {
		mode,
		uid:	int;
		size0:	big;	# use size, below
		atime,
		ctime,
		mtime,
		dtime:	int;
		gid,
		nlinks,
		nblocks,
		flags:	int;
		osd1:	int;
		blockbuf:	array of byte;  # for symbolic links with length name < Syminodemaxlen
		blocks:	array of big;
		pblock,
		ppblock,
		pppblock:	big;
		gen:	int;
		fileacl,
		diracl:	big;
		faddr:	int;
		osd2:	array of byte;
		size:	big;	# based on size, and higher 32 bits if version supports it

		read:	fn(fd: ref Sys->FD, off: big, s: ref Super): (ref Inode, string);
		parse:	fn(buf: array of byte, s: ref Super): (ref Inode, string);
		text:	fn(s: self ref Inode): string;
	};

	Entry: adt {
		inode:	int;
		elen:	int;
		namelen:	int;
		ftype:	int;
		name:	string;

		parse:	fn(buf: array of byte, hasfiletype: int): (ref Entry, string);
		text:	fn(s: self ref Entry): string;
	};

	Inodedir: adt {
		i:	ref Inode;
		offset:	big;	# styx offset

		next:	ref Sys->Dir;
		b:	big;	# current directory block number
		bo:	int;	# offset in block
		buf:	array of byte;	# directory block
	};

	Bits: adt {
		d:	array of byte;

		has:	fn(b: self ref Bits, i: int): int;
	};

	Part: adt {
		fd:	ref Sys->FD;
		s:	ref Super;
		group:	ref Group;
		root:	ref Inode;
		bsize:	int;
		zeroblock:	array of byte;

		init:		fn(fd: ref Sys->FD): (ref Part, string);
		getblock:	fn(p: self ref Part, i: ref Inode, buf: array of byte, bn: big): string;
		inodewalk:	fn(p: self ref Part, i: ref Inode, elem: string): (ref Inode, ref Entry, string);
		inoderead:	fn(p: self ref Part, i: ref Inode, n: int, o: big): (array of byte, string);
		inodeget:	fn(p: self ref Part, i: int): (ref Inode, string);
		inodedir:	fn(p: self ref Part, i: ref Inode): (ref Inodedir, string);
		dirpeek:	fn(p: self ref Part, id: ref Inodedir, o: big): (ref Sys->Dir, string);
		dirnext:	fn(p: self ref Part, id: ref Inodedir, o: big): (ref Sys->Dir, string);
	};
};
