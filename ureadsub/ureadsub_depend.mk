$(OBJDIR)/userio_mod.o : userio_mod.f90 
$(OBJDIR)/urd_progmod.o : urd_progmod.f90 
$(OBJDIR)/uread_arg_count.o : uread_arg_count.f90 
$(OBJDIR)/urd_setprog.o : urd_setprog.f90 $(OBJDIR)/urd_progmod.o 
$(OBJDIR)/urunid.o : urunid.f90 
$(OBJDIR)/urd_getprog.o : urd_getprog.f90 $(OBJDIR)/urd_progmod.o 
$(OBJDIR)/urword.o : urword.for 
$(OBJDIR)/fdcode.o : fdcode.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uspdlo.o : uspdlo.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/upsart.o : upsart.for 
$(OBJDIR)/idcod.o : idcod.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/upquer.o : upquer.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/ureadw.o : ureadw.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/inord.o : inord.for 
$(OBJDIR)/upcget.o : upcget.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uspchk.o : uspchk.for 
$(OBJDIR)/uerjmp.o : uerjmp.for 
$(OBJDIR)/urdcri.o : urdcri.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uflastnb.o : uflastnb.for 
$(OBJDIR)/urdabo.o : urdabo.for 
$(OBJDIR)/uspclr.o : uspclr.for 
$(OBJDIR)/uermsi.o : uermsi.for 
$(OBJDIR)/uspclg.o : uspclg.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/urdstr.o : urdstr.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/ulosup.o : ulosup.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/urdqcd.o : urdqcd.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/usingl.o : usingl.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/usploc.o : usploc.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uready.o : uready.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/r8dcod.o : r8dcod.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/ufdcod.o : ufdcod.for 
$(OBJDIR)/cdcod.o : cdcod.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/udisp1.o : udisp1.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uspunc.o : uspunc.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uset_tty.o : uset_tty.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/urdspf.o : urdspf.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/unixpath.o : unixpath.for 
$(OBJDIR)/utrahd.o : utrahd.for 
$(OBJDIR)/lchar.o : lchar.for 
$(OBJDIR)/uttech.o : uttech.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/ugetrng.o : ugetrng.for 
$(OBJDIR)/ureadr8.o : ureadr8.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/ucbufx.o : ucbufx.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/urdcmc.o : urdcmc.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/neupag.o : neupag.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uinita.o : uinita.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/urdmod.o : urdmod.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uclear.o : uclear.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uprset.o : uprset.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/urdtty.o : urdtty.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/usichk.o : usichk.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/fdcod.o : fdcod.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/ixuopw.o : ixuopw.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/ugmode.o : ugmode.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/utrace.o : utrace.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/urdbuf.o : urdbuf.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uermsr8.o : uermsr8.for 
$(OBJDIR)/udecop.o : udecop.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uppass.o : uppass.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/ur8dcod.o : ur8dcod.for 
$(OBJDIR)/urdopen.o : urdopen.for 
$(OBJDIR)/uiotop.o : uiotop.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uerase.o : uerase.for 
$(OBJDIR)/numclr.o : numclr.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uermsf.o : uermsf.for 
$(OBJDIR)/numfld.o : numfld.for 
$(OBJDIR)/utkset.o : utkset.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/ubinit.o : ubinit.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/urshot.o : urshot.for 
$(OBJDIR)/urdpma.o : urdpma.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/urdcmi.o : urdcmi.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uvideo.o : uvideo.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uindop.o : uindop.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/urwrdl.o : urwrdl.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/iscomp.o : iscomp.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uspget.o : uspget.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/urchar.o : urchar.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/lyes.o : lyes.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/upsinl.o : upsinl.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/ucmppr.o : ucmppr.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/upsadg.o : upsadg.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/jidcod.o : jidcod.for 
$(OBJDIR)/ugetpair.o : ugetpair.for 
$(OBJDIR)/ufilx.o : ufilx.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/upaust.o : upaust.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/urdfcap.o : urdfcap.for 
$(OBJDIR)/urecho.o : urecho.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/ureadi.o : ureadi.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uwrmsg.o : uwrmsg.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/jxuopc.o : jxuopc.for 
$(OBJDIR)/upromx.o : upromx.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/term_check.o : term_check.for 
$(OBJDIR)/chxout.o : chxout.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/urotor.o : urotor.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/lyesc.o : lyesc.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uamode.o : uamode.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/upromt.o : upromt.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uread.o : uread.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uwordl.o : uwordl.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/ldquot.o : ldquot.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/in_ing_list.o : in_ing_list.for 
$(OBJDIR)/ipnblk.o : ipnblk.for 
$(OBJDIR)/idcode.o : idcode.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/intusp.o : intusp.for 
$(OBJDIR)/ulun00.o : ulun00.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uermsg.o : uermsg.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/usplvl.o : usplvl.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/ucbufp.o : ucbufp.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/ufchar.o : ufchar.for 
$(OBJDIR)/uexita.o : uexita.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/urdeof.o : urdeof.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/get_term_int.o : get_term_int.for 
$(OBJDIR)/ugr8pair.o : ugr8pair.for 
$(OBJDIR)/lxout.o : lxout.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/fldxtr.o : fldxtr.for 
$(OBJDIR)/lnout.o : lnout.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/ureadc.o : ureadc.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uspcll.o : uspcll.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/urdpmb.o : urdpmb.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/ureadf.o : ureadf.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/ubline.o : ubline.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/urdch0.o : urdch0.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uppasg.o : uppasg.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/urdspc.o : urdspc.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/upromtx.o : upromtx.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/urxout.o : urxout.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/urstop.o : urstop.for 
$(OBJDIR)/ugbuf0.o : ugbuf0.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uchkop.o : uchkop.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/lcmppr.o : lcmppr.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/ugr8rng.o : ugr8rng.for 
$(OBJDIR)/urdext.o : urdext.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/usplwr.o : usplwr.for 
$(OBJDIR)/upsinx.o : upsinx.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/ucbclr.o : ucbclr.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/usinit.o : usinit.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uacknl.o : uacknl.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/ufiget.o : ufiget.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/ixuopc.o : ixuopc.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/ureadl.o : ureadl.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/urget1.o : urget1.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/upsadl.o : upsadl.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/upromr.o : upromr.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/udecod.o : udecod.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/lword.o : lword.for 
$(OBJDIR)/usplnm.o : usplnm.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uindin.o : uindin.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/r8inord.o : r8inord.for 
$(OBJDIR)/urdprlev.o : urdprlev.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/upmasg.o : upmasg.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/urdtch.o : urdtch.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/ugline.o : ugline.for 
$(OBJDIR)/urcstr.o : urcstr.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/udelim.o : udelim.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/unixfile.o : unixfile.for 
$(OBJDIR)/urderr.o : urderr.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/updisp.o : updisp.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/unamf.o : unamf.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uspdgl.o : uspdgl.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uahead.o : uahead.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/uspcap.o : uspcap.for 
$(OBJDIR)/upssub.o : upssub.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/fldlim.o : fldlim.for 
$(OBJDIR)/urdtti.o : urdtti.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/iscmp0.o : iscmp0.for 
$(OBJDIR)/innblk.o : innblk.for 
$(OBJDIR)/urdstt.o : urdstt.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/urpunc.o : urpunc.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/urdchr.o : urdchr.for $(OBJDIR)/userio_mod.o 
$(OBJDIR)/urstat.o : urstat.for $(OBJDIR)/userio_mod.o 
