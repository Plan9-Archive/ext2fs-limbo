# after initializing the file system, we know the root inode (inode 2 on disk)
# the inode for a file has all meta information for Dir, except the name (that's in the directory contents)
# we can read directory contents of an inode, finding childrens name+inode.
# to get access to all information:
# qid.path is the inode (char/block devices have major,minor in high 16 bits of path;  symlink has high 32 bit ~0),
# fid holds the path elems (strings) and path inodes, so we can navigate and have the fid's name

implement Ext2fs;

include "sys.m";
	sys: Sys;
	sprint: import sys;
include "draw.m";
include "arg.m";
include "bufio.m";
	bufio: Bufio;
	Iobuf: import bufio;
include "string.m";
	str: String;
include "styx.m";
	styx: Styx;
	Tmsg, Rmsg: import styx;
include "lists.m";
	lists: Lists;
include "tables.m";
	tables: Tables;
	Table, Strhash: import tables;
include "../lib/ext2.m";
	ext2: Ext2;
	Super, Group, Inode, Inodedir, Entry, Part: import ext2;

Ext2fs: module {
	init:	fn(nil: ref Draw->Context, args: list of string);
};

Dflag: int;
dflag: int;
Pflag: int;
part: ref Part;
userfile: string;
groupfile: string;

styxfd: ref Sys->FD;
msize: int;

Enotfound:	con "file does not exist";
Eperm:		con "permission denied";
Enoauth:	con "no authentication required";
Einuse:		con "fid already in use";
Ebadfid:	con "bad fid";

Fid: adt {
	mode:	int;
	elems:	list of string;
	inodes:	list of int;
	uname,
	aname:	string;
	inode:	ref Inode;
	inodedir:	ref Inodedir;

	qid:	fn(f: self ref Fid): Sys->Qid;
	stat:	fn(f: self ref Fid): Sys->Dir;
	canread:	fn(f: self ref Fid): int;
	canexec:	fn(f: self ref Fid): int;
};

User: adt {
	u:	string;
	uid:	int;
	gids:	list of int;
};

fidtab: ref Table[ref Fid];
uids: ref Table[ref User];
users: ref Strhash[ref User];
gids: ref Table[string];
ugdone: int;

init(nil: ref Draw->Context, args: list of string)
{
	sys = load Sys Sys->PATH;
	arg := load Arg Arg->PATH;
	bufio = load Bufio Bufio->PATH;
	str = load String String->PATH;
	styx = load Styx Styx->PATH;
	styx->init();
	tables = load Tables Tables->PATH;
	lists = load Lists Lists->PATH;
	ext2 = load Ext2 Ext2->PATH;
	ext2->init();

	fidtab = fidtab.new(32, nil);
	uids = uids.new(32, nil);
	users = users.new(32, nil);
	gids = gids.new(32, nil);

	sys->pctl(Sys->NEWPGRP, nil);

	ugdone = 1;
	mntflags := Sys->MREPL;
	arg->init(args);
	arg->setusage(arg->progname()+" [-dD] [-abc] [-P] [-u userfile] [-g groupfile] ext2file mtpt");
	while((c := arg->opt()) != 0)
		case c {
		'D' =>	Dflag++;
		'd' =>	ext2->dflag = dflag++;
		'a' =>	mntflags = (mntflags & ~Sys->MBEFORE) | Sys->MAFTER;
		'b' =>	mntflags = (mntflags & ~Sys->MAFTER) | Sys->MBEFORE;
		'c' =>	mntflags |= Sys->MCREATE;
		'P' =>	Pflag++;
		'u' =>	userfile = arg->arg();
			ugdone = 0;
		'g' =>	groupfile = arg->arg();
			ugdone = 0;
		* =>	arg->usage();
		}
	args = arg->argv();
	if(len args != 2)
		arg->usage();
	file := hd args;
	mtpt := hd tl args;
	fd := sys->open(file, Sys->OREAD);
	if(fd == nil)
		fail(sprint("open %q: %r", file));
	err: string;
	(part, err) = Part.init(fd);
	if(err != nil)
		fail(err);

	if(sys->pipe(fds := array[2] of ref Sys->FD) < 0)
		fail(sprint("pipe: %r"));
	
	styxfd = fds[0];
	spawn styxreader(styxfd, msgc := chan of ref Tmsg);
	spawn styxsrv(msgc);

	if(sys->mount(fds[1], nil, mtpt, mntflags, nil) < 0)
		fail(sprint("mount: %r"));

	if(userfile != nil)
		readusers();
	if(groupfile != nil)
		readgroups();
	ugdone = 1;
}

readusers()
{
	b := bufio->open(userfile, Bufio->OREAD);
	if(b == nil)
		fail(sprint("open %q: %r", userfile));
	nr := 0;
	for(;;) {
		s := b.gets('\n');
		if(s == nil)
			break;
		nr++;
		l := sys->tokenize(s, ":").t1;
		if(len l < 4)
			fail(sprint("%q:%d: bad line", userfile, nr));
		user := hd l;
		uidstr := hd tl tl l;
		gidstr := hd tl tl tl l;
		(uid, rem0) := str->toint(uidstr, 10);
		(gid, rem1) := str->toint(gidstr, 10);
		if(rem0 != nil || rem1 != nil)
			fail(sprint("%q:%d: bad uid (%#q) or gid (%#q)", userfile, nr, uidstr, gidstr));
		u := ref User (user, uid, gid::nil);
		uids.add(uid, u);
		users.add(user, u);
	}
}

readgroups()
{
	b := bufio->open(groupfile, Bufio->OREAD);
	if(b == nil)
		fail(sprint("open %q: %r", groupfile));
	nr := 0;
	for(;;) {
		s := b.gets('\n');
		if(s == nil)
			break;
		if(s[len s-1] == '\n')
			s = s[:len s-1];
		nr++;
		l := sys->tokenize(s, ":").t1;
		if(len l < 3)
			fail(sprint("%q:%d: bad line", groupfile, nr));
		group := hd l;
		gidstr := hd tl tl l;
		if(len l >= 4)
			ul := sys->tokenize(hd tl tl tl l, ",").t1;
		(gid, rem) := str->toint(gidstr, 10);
		if(rem != nil)
			fail(sprint("%q:%d: bad gid (%#q)", groupfile, nr, gidstr));
		for(; ul != nil; ul = tl ul) {
			u := users.find(hd ul);
			if(u != nil)
				u.gids = gid::u.gids;
			else
				say(sprint("%q:%d: user %#q for group %q (%d) does not exist", groupfile, nr, hd ul, group, gid));
		}
		gids.add(gid, group);
	}
}

getuser(uid: int): string
{
	u := uids.find(uid);
	if(u == nil)
		return string uid;
	return u.u;
}

getgroup(gid: int): string
{
	g := gids.find(gid);
	if(g == nil)
		g = string gid;
	return g;
}

g16(d: array of byte): int
{
	return int d[0]|int d[1]<<8;
}

Fid.qid(f: self ref Fid): Sys->Qid
{
	qtype := Sys->QTFILE;
	path := big hd f.inodes;
	case f.inode.mode & ext2->IMfilemask {
	ext2->IMdir =>
		qtype = Sys->QTDIR;
	ext2->IMblockdev or
	ext2->IMchardev =>
		majmin := big g16(f.inode.blockbuf[0:]);
		path |= majmin<<48;
	ext2->IMsymlink =>
		path |= big ~0<<32;
	}
	return Sys->Qid (path, f.inode.gen, qtype);
}

Fid.stat(f: self ref Fid): Sys->Dir
{
	i := f.inode;
	mode := i.mode & ext2->IMmodemask;
	if((i.mode & ext2->IMfilemask) == ext2->IMdir)
		mode |= Sys->DMDIR;
	return Sys->Dir (hd f.elems, getuser(i.uid), getgroup(i.gid), "", f.qid(), mode, i.atime, i.mtime, i.size, 0, 0);
}

Fid.canread(f: self ref Fid): int
{
	i := f.inode;
	m := i.mode;
	return Pflag || !ugdone || f.uname == getuser(i.uid) && (m & 8r400) || f.uname == getgroup(i.gid) && (m & 8r040) || (m & 8r004);
}

Fid.canexec(f: self ref Fid): int
{
	i := f.inode;
	m := i.mode;
	return Pflag || !ugdone || f.uname == getuser(i.uid) && (m & 8r500) == 8r500 || f.uname == getgroup(i.gid) && (m & 8r050) == 8r050 || (m & 8r005) == 8r005;
}

zeroqid: Sys->Qid;
walk(f: ref Fid, name: string): (Sys->Qid, string)
{
	if(name == "..") {
		if(len f.elems > 1) {
			f.elems = tl f.elems;
			f.inodes = tl f.inodes;
			err: string;
			(f.inode, err) = part.inodeget(hd f.inodes);
			if(err != nil)
				return (zeroqid, err);
		}
	} else {
		(i, e, err) := part.inodewalk(f.inode, name);
		if(err == nil && i == nil)
			err = Enotfound;
		if(err != nil)
			return (zeroqid, err);
		f.elems = e.name::f.elems;
		f.inodes = e.inode::f.inodes;
		f.inode = i;
	}
	return (f.qid(), nil);
}


styxreader(fd: ref Sys->FD, msgc: chan of ref Tmsg)
{
	for(;;) {
		m := Tmsg.read(fd, styx->MAXRPC);
		if(m != nil && Dflag)
			warn("<- "+m.text());
		msgc <-= m;
		if(m == nil)
			break;
	}
}

styxsrv(msgc: chan of ref Tmsg)
{
Done:
	for(;;) {
		mm := <-msgc;
		if(mm == nil)
			break Done;
		pick m := mm {
		Readerror =>
			warn("read error: "+m.error);
			break Done;
		}
		dostyx(mm);
	}
	killgrp(sys->pctl(0, nil));
}

dostyx(mm: ref Tmsg)
{
	pick m := mm {
	Version =>
		version: string;
		(msize, version) = styx->compatible(m, styx->MAXRPC, nil);
		reply(ref Rmsg.Version (m.tag, msize, version));
		# should we clear state if we've done version before?

	Auth =>
		return replyerror(m, Enoauth);

	Attach =>
		if(m.afid != styx->NOFID)
			return replyerror(m, Enoauth);
		if(fidtab.find(m.fid) != nil)
			return replyerror(m, Einuse);
		nf := ref Fid (0, "/"::nil, 2::nil, m.uname, m.aname, part.root, nil);
		fidtab.add(m.fid, nf);
		reply(ref Rmsg.Attach (m.tag, nf.qid()));

	Flush =>
		reply(ref Rmsg.Flush (m.tag));

	Walk =>
		if(m.newfid != m.fid && fidtab.find(m.newfid) != nil)
			return replyerror(m, Einuse);
		f := fidtab.find(m.fid);
		if(f == nil || f.mode)
			return replyerror(m, Ebadfid);
		nf := ref *f;
		err: string;
		qids := array[len m.names] of Sys->Qid;
		for(i := 0; err == nil && i < len m.names; i++)
			(qids[i], err) = walk(nf, m.names[i]);
		if(err != nil)
			return replyerror(m, err);
		fidtab.del(m.newfid);
		fidtab.add(m.newfid, nf);
		reply(ref Rmsg.Walk (m.tag, qids));

	Open =>
		f := fidtab.find(m.fid);
		if(f == nil || f.mode)
			return replyerror(m, Ebadfid);
		modeok := m.mode == styx->OREAD && f.canread() || m.mode == styx->OEXEC && f.canread() && f.canexec();
		if(!modeok)
			return replyerror(m, Eperm);
		f.mode = styx->OREAD;
		reply(ref Rmsg.Open (m.tag, f.qid(), styx->MAXFDATA));

	Create =>
		return replyerror(m, Eperm);

	Read =>
		f := fidtab.find(m.fid);
		if(f == nil || f.mode != styx->OREAD)
			return replyerror(m, Ebadfid);

		if((f.inode.mode & ext2->IMfilemask) == ext2->IMdir) {
			if(f.inodedir == nil) {
				if(m.offset != big 0)
					return replyerror(m, "bad directory offset");
				err: string;
				(f.inodedir, err) = part.inodedir(f.inode);
				if(err != nil)
					return replyerror(m, err);
			}
			max := msize-styx->IOHDRSZ;
			l: list of array of byte;
			n := 0;
			offset := m.offset;
			for(;;) {
				(dir, err) := part.dirpeek(f.inodedir, offset);
				if(err != nil)
					return replyerror(m, err);
				if(dir == nil)
					break;
				dir.uid = getuser(int dir.uid);
				dir.gid = getgroup(int dir.gid);
				nb := styx->packdir(*dir);
				if(n+len nb > max)
					break;
				part.dirnext(f.inodedir, offset);
				f.inodedir.offset = offset += big len nb;
				n += len nb;
				l = nb::l;
			}
			buf := array[n] of byte;
			o := 0;
			for(l = lists->reverse(l); l != nil; l = tl l) {
				buf[o:] = hd l;
				o += len hd l;
			}
			return reply(ref Rmsg.Read (m.tag, buf));
		}

		(buf, err) := part.inoderead(f.inode, m.count, m.offset);
		if(err != nil)
			return replyerror(m, err);
		reply(ref Rmsg.Read (m.tag, buf));

	Write =>
		return replyerror(m, Eperm);

	Clunk =>
		if(!fidtab.del(m.fid))
			return replyerror(m, Ebadfid);
		return reply(ref Rmsg.Clunk (m.tag));

	Stat =>
		f := fidtab.find(m.fid);
		if(f == nil)
			return replyerror(m, Ebadfid);
		reply(ref Rmsg.Stat (m.tag, f.stat()));

	Remove =>
		if(!fidtab.del(m.fid))
			return replyerror(m, Ebadfid);
		return replyerror(m, Eperm);

	Wstat =>
		return replyerror(m, Eperm);
	}
}

replyerror(m: ref Tmsg, s: string)
{
	reply(ref Rmsg.Error(m.tag, s));
}

reply(m: ref Rmsg)
{
	if(m != nil && Dflag)
		warn("-> "+m.text());
	sys->write(styxfd, buf := m.pack(), len buf);
}

killgrp(pid: int)
{
	fd := sys->open(sprint("/prog/%d/ctl", pid), Sys->OWRITE);
	sys->fprint(fd, "killgrp");
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
	killgrp(sys->pctl(0, nil));
	raise "fail:"+s;
}
