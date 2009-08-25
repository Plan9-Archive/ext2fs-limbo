implement Ext2;

include "sys.m";
	sys: Sys;
	print, sprint: import sys;
include "ext2.m";

Superoff:	con 1024;

init()
{
	sys = load Sys Sys->PATH;
}

Super.parse(buf: array of byte): (ref Super, string)
{
	s := ref Super;
	o := 0;
	{
		(s.ninodes, o)		= g32(buf, o);
		(s.nblocks, o)		= g32(buf, o);
		(s.nrblocks, o)		= g32(buf, o);
		(s.nfreeblocks, o)	= g32(buf, o);
		(s.nfreeinodes, o)	= g32(buf, o);
		(s.firstdblock, o)	= g32(buf, o);
		(s.blocklog, o)		= g32i(buf, o);
		(s.fraglog, o)		= g32i(buf, o);
		(s.ngroupblocks, o)	= g32i(buf, o);
		(s.ngroupfrags, o)	= g32i(buf, o);
		(s.ngroupinodes, o)	= g32i(buf, o);
		(s.mtime, o)		= g32i(buf, o);
		(s.wtime, o)		= g32i(buf, o);
		(s.nmnt, o)		= g16(buf, o);
		(s.maxmnt, o)		= g16(buf, o);
		(s.magic, o)		= g16(buf, o);
		(s.state, o)		= g16(buf, o);
		(s.errors, o)		= g16(buf, o);
		(s.minorrev, o)		= g16(buf, o);
		(s.lastcheck, o)	= g32i(buf, o);
		(s.checkival, o)	= g32i(buf, o);
		(s.creator, o)		= g32i(buf, o);
		(s.rev, o)		= g32i(buf, o);
		(s.defresuid, o)	= g16(buf, o);
		(s.defresgid, o)	= g16(buf, o);

		if(s.rev == SRdynamic) {
			(s.firstinode, o)	= g32i(buf, o);
			(s.inodesize, o)	= g16(buf, o);
			(s.blockgroup, o)	= g16(buf, o);
			(s.featcompat, o)	= g32i(buf, o);
			(s.featincompat, o)	= g32i(buf, o);
			(s.featrocompat, o)	= g32i(buf, o);
			(s.uuid, o)		= gbuf(buf, o, 16);
			(s.volumename, o)	= gbuf(buf, o, 16);
			(s.lastmounted, o)	= gbuf(buf, o, 64);
			(s.bitmapalg, o)	= g32i(buf, o);
		}

		o = 204;
		(s.preallocblocks, o)		= g8(buf, o);
		(s.preallocdirblocks, o)	= g8(buf, o);

		o = 208;
		(s.journaluuid, o)	= gbuf(buf, o, 16);
                (s.journalinum, o)	= g32i(buf, o);
                (s.journaldev, o)	= g32i(buf, o);
                (s.lastorphan, o)	= g32i(buf, o);

                s.hashseed = array[4] of int;
		(s.hashseed[0], o)	= g32i(buf, o);
		(s.hashseed[1], o)	= g32i(buf, o);
		(s.hashseed[2], o)	= g32i(buf, o);
		(s.hashseed[3], o)	= g32i(buf, o);
                (s.hashversion, o)	= g32i(buf, o);

                (s.defmntopts, o)	= g32i(buf, o);
                (s.firstmetabg, o)	= g32i(buf, o);
	} exception {
	"array bounds error" =>
		return (nil, sprint("reading past buffer at offset %d, length %d", o, len buf));
	}
	if(s.magic != Magic)
		return (nil, sprint("bad magic, expected %04ux, saw %04ux", Magic, s.magic));
	return (s, nil);
}

compatstrs := array[] of {
"Dirprealloc", "Imagicinodes", "Hasjournal", "Extattr", "Resizeino", "Dirindex",
};
incompatstrs := array[] of {
"Compression", "Filetype", "Recover", "Journaldev", "Metabg",
};
rocompatstrs := array[] of {
"Sparsesuper", "Largefile", "Btreedir",
};

compatstr(v: int, a: array of string): string
{
	s := "";
	for(i := 0; i < len a; i++)
		if((1<<i) & v) {
			s += ","+a[i];
			v &= ~(1<<i);
		}
	if(v != 0)
		s += sprint("Other(%#ux)", v);
	if(s != nil)
		s = s[1:];
	return s;
}

featcompatstr(v: int): string
{
	return compatstr(v, compatstrs);
}

featincompatstr(v: int): string
{
	return compatstr(v, incompatstrs);
}

featrocompatstr(v: int): string
{
	return compatstr(v, rocompatstrs);
}


Super.read(fd: ref Sys->FD, off: big): (ref Super, string)
{
	buf := array[Superlen] of byte;
	n := preadn(fd, buf, len buf, off);
	if(n < 0)
		return (nil, sprint("read: %r"));
	if(n != len buf)
		return (nil, sprint("short read, want %d, have %d", len buf, n));
	return Super.parse(buf);
}

Super.text(s: self ref Super): string
{
	r := "Super:\n";
	r += sprint("\t%-15s %bd\n", "ninodes", s.ninodes);
	r += sprint("\t%-15s %bd\n", "nblocks", s.nblocks);
	r += sprint("\t%-15s %bd\n", "nrblocks", s.nrblocks);
	r += sprint("\t%-15s %bd\n", "nfreeblocks", s.nfreeblocks);
	r += sprint("\t%-15s %bd\n", "nfreeinodes", s.nfreeinodes);
	r += sprint("\t%-15s %bd\n", "firstdblock", s.firstdblock);
	r += sprint("\t%-15s %d\n", "blocklog", s.blocklog);
	r += sprint("\t%-15s %d\n", "fraglog", s.fraglog);
	r += sprint("\t%-15s %d\n", "ngroupblocks", s.ngroupblocks);
	r += sprint("\t%-15s %d\n", "ngroupfrags", s.ngroupfrags);
	r += sprint("\t%-15s %d\n", "ngroupinodes", s.ngroupinodes);
	r += sprint("\t%-15s %d\n", "mtime", s.mtime);
	r += sprint("\t%-15s %d\n", "wtime", s.wtime);
	r += sprint("\t%-15s %d\n", "nmnt", s.nmnt);
	r += sprint("\t%-15s %d\n", "maxmnt", s.maxmnt);
	r += sprint("\t%-15s %04ux\n", "magic", s.magic);
	r += sprint("\t%-15s %d\n", "state", s.state);
	r += sprint("\t%-15s %d\n", "errors", s.errors);
	r += sprint("\t%-15s %d\n", "minorrev", s.minorrev);
	r += sprint("\t%-15s %d\n", "lastcheck", s.lastcheck);
	r += sprint("\t%-15s %d\n", "checkival", s.checkival);
	r += sprint("\t%-15s %d\n", "creator", s.creator);
	r += sprint("\t%-15s %d\n", "rev", s.rev);
	r += sprint("\t%-15s %d\n", "defresuid", s.defresuid);
	r += sprint("\t%-15s %d\n", "defresgid", s.defresgid);
	if(s.rev == SRdynamic) {
		r += "Super Dynamic:\n";
		r += sprint("\t%-15s %d\n", "firstinode", s.firstinode);
		r += sprint("\t%-15s %d\n", "inodesize", s.inodesize);
		r += sprint("\t%-15s %d\n", "blockgroup", s.blockgroup);
		r += sprint("\t%-15s %#ux (%s)\n", "featcompat", s.featcompat, featcompatstr(s.featcompat));
		r += sprint("\t%-15s %#ux (%s)\n", "featincompat", s.featincompat, featincompatstr(s.featincompat));
		r += sprint("\t%-15s %#ux (%s)\n", "featrocompat", s.featrocompat, featrocompatstr(s.featrocompat));
		r += sprint("\t%-15s %q\n", "uuid", hex(s.uuid));
		r += sprint("\t%-15s %q\n", "volumename", string s.volumename);  # latin1?
		r += sprint("\t%-15s %q\n", "lastmounted", string s.lastmounted);  # latin1?
		r += sprint("\t%-15s %d\n", "bitmapalg", s.bitmapalg);
	} else {
		s.firstinode = 11;
		s.inodesize = 128;
	}
	r += sprint("\t%-15s %d\n", "preallocblocks", s.preallocblocks);
	r += sprint("\t%-15s %d\n", "preallocdirblocks", s.preallocdirblocks);

	if(s.featcompat & CompatHasjournal) {
		r += sprint("\t%-15s %s\n", "journaluuid", hex(s.journaluuid));
		r += sprint("\t%-15s %d\n", "journalinum", s.journalinum);
		r += sprint("\t%-15s %d\n", "journaldev", s.journaldev);
		r += sprint("\t%-15s %d\n", "lastorphan", s.lastorphan);
	}

	if(s.featcompat & CompatDirindex ) {
		r += sprint("\t%-15s %ux %ux %ux %ux\n", "hashseed", s.hashseed[0], s.hashseed[1], s.hashseed[2], s.hashseed[3]);
		r += sprint("\t%-15s %d\n", "hashversion", s.hashversion);
	}

	r += sprint("\t%-15s %#ux\n", "defmntopts", s.defmntopts);
	if(s.featincompat & IncompatMetabg)
		r += sprint("\t%-15s %d\n", "firstmetabg", s.firstmetabg);
	return r;
}


Group.read(fd: ref Sys->FD, off: big): (ref Group, string)
{
	buf := array[Grouplen] of byte;
	n := preadn(fd, buf, len buf, off);
	if(n < 0)
		return (nil, sprint("read: %r"));
	if(n != len buf)
		return (nil, sprint("short read, want %d, got %d", len buf, n));
	return Group.parse(buf);
}

Group.parse(buf: array of byte): (ref Group, string)
{
	g := ref Group;
	o := 0;
	{
		(g.blockbitmap, o)	= g32(buf, o);
		(g.inodebitmap, o)	= g32(buf, o);
		(g.inodetab, o)		= g32(buf, o);
		(g.nfreeblocks, o)	= g16(buf, o);
		(g.nfreeinodes, o)	= g16(buf, o);
		(g.nuseddirs, o)	= g16(buf, o);
		o += 2;
		(g.reserved, o)		= gbuf(buf, o, 12);
	} exception {
	"array bounds error" =>
		return (nil, sprint("reading past buffer at offset %d, length %d", o, len buf));
	}
	return (g, nil);
}

Group.text(g: self ref Group): string
{
	r := "Group:\n";
	r += sprint("\t%-15s %bd\n", "blockbitmap", g.blockbitmap);
	r += sprint("\t%-15s %bd\n", "inodebitmap", g.inodebitmap);
	r += sprint("\t%-15s %bd\n", "inodetab", g.inodetab);
	r += sprint("\t%-15s %d\n", "nfreeblocks", g.nfreeblocks);
	r += sprint("\t%-15s %d\n", "nfreeinodes", g.nfreeinodes);
	r += sprint("\t%-15s %d\n", "nuseddirs", g.nuseddirs);
	r += sprint("\t%-15s %s\n", "reserved", hex(g.reserved));
	return r;
}


Inode.read(fd: ref Sys->FD, off: big, s: ref Super): (ref Inode, string)
{
	buf := array[s.inodesize] of byte;
	n := preadn(fd, buf, len buf, off);
	if(n < 0)
		return (nil, sprint("read: %r"));
	if(n != len buf)
		return (nil, sprint("short read, want %d, got %d", len buf, n));
	return Inode.parse(buf, s);
}

Inode.parse(buf: array of byte, s: ref Super): (ref Inode, string)
{
	i := ref Inode;
	o := 0;
	{
		(i.mode, o)	= g16(buf, o);
		(i.uid, o)	= g16(buf, o);
		(i.size0, o)	= g32(buf, o);
		(i.atime, o)	= g32i(buf, o);
		(i.ctime, o)	= g32i(buf, o);
		(i.mtime, o)	= g32i(buf, o);
		(i.dtime, o)	= g32i(buf, o);
		(i.gid, o)	= g16(buf, o);
		(i.nlinks, o)	= g16(buf, o);
		(i.nblocks, o)	= g32i(buf, o);
		(i.flags, o)	= g32i(buf, o);
		(i.osd1, o)	= g32i(buf, o);
		i.blockbuf = array[60] of byte;
		i.blockbuf[:] = buf[o:o+60];
		i.blocks = array[12] of big;
		for(j := 0; j < 12; j++)
			(i.blocks[j], o) = g32(buf, o);
		(i.pblock, o)	= g32(buf, o);
		(i.ppblock, o)	= g32(buf, o);
		(i.pppblock, o)	= g32(buf, o);
		(i.gen, o)	= g32i(buf, o);
		(i.fileacl, o)	= g32(buf, o);
		(i.diracl, o)	= g32(buf, o);
		(i.faddr, o)	= g32i(buf, o);
		(i.osd2, o)	= gbuf(buf, o, 12);
		i.size = i.size0;
		if(s.rev > 0)
			i.size |= i.diracl<<32;
	} exception {
	"array bounds error" =>
		return (nil, sprint("reading past buffer at offset %d, length %d", o, len buf));
	}
	return (i, nil);
}

Inode.text(i: self ref Inode): string
{
	r := "Inode\n";
	r += sprint("\t%-15s %o\n", "mode", i.mode);
	r += sprint("\t%-15s %d\n", "uid", i.uid);
	r += sprint("\t%-15s %bd\n", "size0", i.size0);
	r += sprint("\t%-15s %d\n", "atime", i.atime);
	r += sprint("\t%-15s %d\n", "ctime", i.ctime);
	r += sprint("\t%-15s %d\n", "mtime", i.mtime);
	r += sprint("\t%-15s %d\n", "dtime", i.dtime);
	r += sprint("\t%-15s %d\n", "gid", i.gid);
	r += sprint("\t%-15s %d\n", "nlinks", i.nlinks);
	r += sprint("\t%-15s %d\n", "nblocks", i.nblocks);
	r += sprint("\t%-15s %ux\n", "flags", i.flags);
	r += sprint("\t%-15s %d\n", "osd1", i.osd1);
	r += sprint("\t%-15s", "blocks");
	b := i.blocks;
	for(j := 0; j < len b; j++)
		r += sprint(" %bd ", b[j]);
	r += "\n";
	r += sprint("\t%-15s %bd\n", "pblock", i.pblock);
	r += sprint("\t%-15s %bd\n", "ppblock", i.ppblock);
	r += sprint("\t%-15s %bd\n", "pppblock", i.pppblock);
	r += sprint("\t%-15s %d\n", "gen", i.gen);
	r += sprint("\t%-15s %bd\n", "fileacl", i.fileacl);
	r += sprint("\t%-15s %bd\n", "diracl", i.diracl);
	r += sprint("\t%-15s %d\n", "faddr", i.faddr);
	r += sprint("\t%-15s %s\n", "osd2", hex(i.osd2));
	r += sprint("\t%-15s %bd\n", "size", i.size);
	return r;
}


Entry.parse(buf: array of byte, hasfiletype: int): (ref Entry, string)
{
	e := ref Entry;
	o := 0;
	{
		(e.inode, o) = g32i(buf, o);
		(e.elen, o) = g16(buf, o);
		if(hasfiletype) {
			(e.namelen, o) = g8(buf, o);
			(e.ftype, o) = g8(buf, o);
		} else {
			(e.namelen, o) = g16(buf, o);
			e.ftype = IFTunknown;
		}
		name: array of byte;
		(name, o) = gbuf(buf, o, e.namelen);
		# name is latin1?
		e.name = string name;
	} exception {
	"array bounds error" =>
		return (nil, sprint("reading past buffer at offset %d, length %d", o, len buf));
	}
	return (e, nil);
}

Entry.text(e: self ref Entry): string
{
	r := "Entry\n";
	r += sprint("\t%-15s %d\n", "inode", e.inode);
	r += sprint("\t%-15s %d\n", "elen", e.elen);
	r += sprint("\t%-15s %d\n", "namelen", e.namelen);
	r += sprint("\t%-15s %d\n", "ftype", e.ftype);
	r += sprint("\t%-15s %q\n", "name", e.name);
	return r;
}

Bits.has(b: self ref Bits, i: int): int
{
	return int b.d[i/8] & (1<<(7-(i&7)));
}

Part.init(fd: ref Sys->FD): (ref Part, string)
{
	(s, err) := Super.read(fd, big Superoff);
	if(err != nil)
		return (nil, "reading primary super block: "+err);

	if(s.state == SSunclean) {
		err = "unclean file system, not properly unmounted";
		if(s.errors == SEpanic)
			return (nil, err);
		warn(err);
	}

	if((s.featincompat & ~(IncompatFiletype)) != 0)
		return (nil, "incompatible features used: "+featincompatstr(s.featincompat));

	bsize := 1024<<s.blocklog;

	g: ref Group;
	(g, err) = Group.read(fd, big bsize * big ((Superoff+Superlen+bsize-1)/bsize));
	if(err != nil)
		return (nil, "reading primary block group descriptor: "+err);

	inodebase := g.inodetab*big bsize;
	root: ref Inode;
	(root, err) = Inode.read(fd, inodebase+big (1*s.inodesize), s);
	if(err != nil)
		return (nil, "reading inode for root: "+err);

	return (ref Part (fd, s, g, root, bsize, nil), nil);
}

Part.getblock(p: self ref Part, i: ref Inode, buf: array of byte, bn: big): string
{
	if(bn*big p.bsize > i.size)
		return sprint("read past last block");

	err: string;
	n0 := big len i.blocks;
	nind := big p.bsize/big 4;
	if(bn < n0) {
		err = readblock(p, buf, i.blocks[int bn]);
	} else if(bn < n0+nind) {
		bn -= n0;
		err = readblock(p, buf, i.pblock);
		if(err == nil) err = readblock(p, buf, g32(buf, int bn*4).t0);
	} else if(bn < n0+nind+nind*nind) {
		bn -= n0+nind;
		err = readblock(p, buf, i.ppblock);
		if(err == nil) err = readblock(p, buf, g32(buf, int (bn / nind)*4).t0);
		if(err == nil) err = readblock(p, buf, g32(buf, int (bn % nind)*4).t0);
	} else if(bn < n0+nind+nind*nind+nind*nind*nind) {
		bn -= n0+nind+nind*nind;
		err = readblock(p, buf, i.pppblock);
		if(err == nil) err = readblock(p, buf, g32(buf, int (bn / (nind*nind))*4).t0);
		if(err == nil) err = readblock(p, buf, g32(buf, int ((bn % (nind*nind))/nind)*4).t0);
		if(err == nil) err = readblock(p, buf, g32(buf, int (bn % nind)*4).t0);
	} else
		err = "file too big";
	return err;
}

readblock(p: ref Part, buf: array of byte, bn: big): string
{
	if(bn == big 0) {
		if(p.zeroblock == nil)
			p.zeroblock = array[p.bsize] of {* => byte 0};
		buf[:] = p.zeroblock;
		return nil;
	}

	n := preadn(p.fd, buf, p.bsize, bn*big p.bsize);
	if(n != p.bsize)
		return sprint("bad/short read: want %d, got %d, bn %bd, %r", p.bsize, n, bn);
	return nil;
}

Part.inodewalk(p: self ref Part, i: ref Inode, elem: string): (ref Inode, ref Entry, string)
{
	end := (i.size+big p.bsize-big 1)/big p.bsize;
	for(bn := big 0; bn < end; bn++) {
		err := p.getblock(i, buf := array[p.bsize] of byte, bn);
		if(err != nil)
			return (nil, nil, err);
		oo := 0;
		while(oo+Entryminlen <= len buf) {
			e: ref Entry;
			(e, err) = Entry.parse(buf[oo:], p.s.featincompat & IncompatFiletype);
			if(err != nil)
				return (nil, nil, err);
			if(elem == e.name) {
				n: ref Inode;
				(n, err) = p.inodeget(e.inode);
				return (n, e, err);
			}
			oo += e.elen;
		}
	}
	return (nil, nil, nil);
}

Part.inoderead(p: self ref Part, i: ref Inode, n: int, o: big): (array of byte, string)
{
	if((i.mode & IMfilemask) == IMsymlink && i.size <= big Syminodesizemax) {
		e := o+big n;
		if(e > i.size)
			e = i.size;
		if(o < big 0)
			o = big 0;
		return (i.blockbuf[int o:int e], nil);
	}

	case i.mode & IMfilemask {
	IMsocket =>	return (nil, "file is a socket");
	IMblockdev =>	return (nil, "file is a block device");
	IMchardev =>	return (nil, "file is a character device");
	IMfifo =>	return (nil, "file is a fifo");
	}

	# we stop on block boundary so next (sequential) read is block-aligned
	mask := ~big (p.bsize-1);
	s := o & mask;
	e := (o+big n) & mask;
	if(o == e)
		e = o+big n;
	if(e > i.size)
		e = i.size;
	size := int (e-s);
	if(size < p.bsize)
		size = p.bsize;
	buf := array[size] of byte;
	bufp := buf;
	end := (e+big p.bsize-big 1)/big p.bsize;
	for(c := s/big p.bsize; c < end; c++) {
		err := p.getblock(i, bufp, c);
		if(err != nil)
			return (nil, err);
		if(p.bsize <= len bufp)
			bufp = bufp[p.bsize:];
	}
	return (buf[int (o-s):int (e-s)], nil);
}

Part.inodeget(p: self ref Part, i: int): (ref Inode, string)
{
	g := (i-1)/p.s.ngroupinodes;
	o := (big g*big p.s.ngroupblocks + p.group.inodetab) * big p.bsize;
	li := (i-1) & ((p.s.ngroupinodes)-1);
	o += big (li*p.s.inodesize);
	return Inode.read(p.fd, o, p.s);
}

Part.inodedir(nil: self ref Part, i: ref Inode): (ref Inodedir, string)
{
	return (ref Inodedir (i, big 0, nil, big 0, 0, nil), nil);
}

Part.dirpeek(p: self ref Part, id: ref Inodedir, o: big): (ref Sys->Dir, string)
{
	if(id.next != nil)
		return (id.next, nil);
	if(o != id.offset)
		return (nil, sprint("bad directory offset1, %bd != %bd", o, id.offset));

	err: string;
	for(;;) {
		if(id.bo+Entryminlen > p.bsize) {
			id.bo = 0;
			id.b++;
			id.buf = nil;
		}
		if(id.buf == nil) {
			if(id.b*big p.bsize >= id.i.size)
				break;
			id.buf = array[p.bsize] of byte;
			err = p.getblock(id.i, id.buf, id.b);
		}

		e: ref Entry;
		n: ref Inode;
		(e, err) = Entry.parse(id.buf[id.bo:], p.s.featincompat & IncompatFiletype);
		if(err == nil)
			id.bo += e.elen;
		if(err == nil && (e.inode == 0 || e.name == "." || e.name == ".."))
			continue;
		if(err == nil)
			(n, err) = p.inodeget(e.inode);
		if(err == nil)
			id.next = ref inode2dir(e.name, e.inode, n);
		break;
	}
	return (id.next, err);
}

Part.dirnext(p: self ref Part, id: ref Inodedir, o: big): (ref Sys->Dir, string)
{
	if(id.next == nil)
		(dir, err) := p.dirpeek(id, o);
	else if(o != id.offset)
		err = sprint("bad directory offset, %bd != %bd", o, id.offset);

	if(err == nil) {
		dir = id.next;
		id.next = nil;
		id.offset = o;
	}
	return (dir, err);
}

inode2dir(name: string, i: int, n: ref Inode): Sys->Dir
{
	mode := n.mode & IMmodemask;
	qt := Sys->QTFILE;
	qid := Sys->Qid (big i, n.gen, qt);
	case n.mode & IMfilemask {
	IMdir =>
		mode |= Sys->DMDIR;
		qt = Sys->QTDIR;
	IMblockdev or
	IMchardev =>
		qid.path |= big g16(n.blockbuf, 0).t0<<48;
	IMsymlink =>
		qid.path |= big ~0<<32;
	}
	return Sys->Dir (name, string n.uid, string n.gid, "", qid, mode, n.atime, n.mtime, n.size, 0, 0);
}

g8(d: array of byte, o: int): (int, int)
{
	v := int d[o++];
	return (v, o);
}

g16(d: array of byte, o: int): (int, int)
{
	v := 0;
	v |= int d[o++]<<0;
	v |= int d[o++]<<8;
	return (v, o);
}

g32(d: array of byte, o: int): (big, int)
{
	v := big 0;
	v |= big d[o++]<<0;
	v |= big d[o++]<<8;
	v |= big d[o++]<<16;
	v |= big d[o++]<<24;
	return (v, o);
}

g32i(d: array of byte, o: int): (int, int)
{
	v: big;
	(v, o) = g32(d, o);
	return (int v, o);
}

gbuf(d: array of byte, o: int, n: int): (array of byte, int)
{
	r := array[n] of byte;
	r[:] = d[o:o+n];
	return (r, o+n);
}

preadn(fd: ref Sys->FD, buf: array of byte, n: int, o: big): int
{
	h := 0;
	while(h < n) {
		nn := sys->pread(fd, buf[h:], n-h, o+big h);
		if(nn < 0)
			return nn;
		if(nn == 0)
			break;
		h += nn;
	}
	return h;
}

hex(d: array of byte): string
{
	s := "";
	for(i := 0; i < len d; i++)
		s += sprint("%02x", int d[i]);
	return s;
}

warn(s: string)
{
	sys->fprint(sys->fildes(2), "ext2: %s\n", s);
}

say(s: string)
{
	if(dflag)
		warn(s);
}
