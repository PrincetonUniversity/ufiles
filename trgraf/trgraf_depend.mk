$(OBJDIR)/bottoms_up.o : bottoms_up.f90 
$(OBJDIR)/trgraf_wname_save.o : trgraf_wname_save.f90 
$(OBJDIR)/gftic.o : gftic.f90 
$(OBJDIR)/mulfdr_blk.o : mulfdr_blk.f90 
$(OBJDIR)/gscale_blk.o : gscale_blk.f90 
$(OBJDIR)/intlab.o : intlab.f90 
$(OBJDIR)/gaxes_blk.o : gaxes_blk.f90 
$(OBJDIR)/plot1mmx.o : plot1mmx.f90 
$(OBJDIR)/bottoms_up_sub.o : bottoms_up_sub.f90 $(OBJDIR)/bottoms_up.o 
$(OBJDIR)/trgraf_slist.o : trgraf_slist.f90 
$(OBJDIR)/r8_mulgraf.o : r8_mulgraf.f90 
$(OBJDIR)/conrdf.o : conrdf.for 
$(OBJDIR)/gscsmg.o : gscsmg.for $(OBJDIR)/plot1mmx.o $(OBJDIR)/gscale_blk.o $(OBJDIR)/gaxes_blk.o 
$(OBJDIR)/grafxe.o : grafxe.for $(OBJDIR)/plot1mmx.o $(OBJDIR)/gscale_blk.o 
$(OBJDIR)/consel.o : consel.for 
$(OBJDIR)/condom.o : condom.for $(OBJDIR)/plot1mmx.o 
$(OBJDIR)/grflbl.o : grflbl.for 
$(OBJDIR)/gftic0.o : gftic0.for $(OBJDIR)/gftic.o 
$(OBJDIR)/gfticx.o : gfticx.for $(OBJDIR)/gftic.o 
$(OBJDIR)/gmmsel.o : gmmsel.for 
$(OBJDIR)/grfcnt.o : grfcnt.for 
$(OBJDIR)/gscrec.o : gscrec.for $(OBJDIR)/plot1mmx.o $(OBJDIR)/gscale_blk.o 
$(OBJDIR)/spredm.o : spredm.for 
$(OBJDIR)/gaxcur.o : gaxcur.for $(OBJDIR)/gaxes_blk.o 
$(OBJDIR)/gscgdf.o : gscgdf.for $(OBJDIR)/gscale_blk.o 
$(OBJDIR)/dplofr.o : dplofr.for $(OBJDIR)/plot1mmx.o 
$(OBJDIR)/gscmmx.o : gscmmx.for $(OBJDIR)/gscale_blk.o $(OBJDIR)/gaxes_blk.o 
$(OBJDIR)/grfmm1b.o : grfmm1b.for $(OBJDIR)/plot1mmx.o 
$(OBJDIR)/grf3x2.o : grf3x2.for 
$(OBJDIR)/immnb.o : immnb.for $(OBJDIR)/plot1mmx.o 
$(OBJDIR)/grafx1.o : grafx1.for $(OBJDIR)/plot1mmx.o $(OBJDIR)/gscale_blk.o 
$(OBJDIR)/conlbl.o : conlbl.for $(OBJDIR)/intlab.o 
$(OBJDIR)/gscsep.o : gscsep.for 
$(OBJDIR)/ufhelp.o : ufhelp.for 
$(OBJDIR)/ifixr.o : ifixr.for 
$(OBJDIR)/gscgt1.o : gscgt1.for 
$(OBJDIR)/tgrf3d.o : tgrf3d.for 
$(OBJDIR)/grf3f2.o : grf3f2.for 
$(OBJDIR)/r8_trgraf.o : r8_trgraf.for 
$(OBJDIR)/grafx2.o : grafx2.for $(OBJDIR)/plot1mmx.o $(OBJDIR)/gscale_blk.o 
$(OBJDIR)/mulfdr.o : mulfdr.for $(OBJDIR)/plot1mmx.o $(OBJDIR)/mulfdr_blk.o $(OBJDIR)/bottoms_up.o 
$(OBJDIR)/gscdef.o : gscdef.for $(OBJDIR)/plot1mmx.o $(OBJDIR)/gscale_blk.o 
$(OBJDIR)/grfssym2.o : grfssym2.for $(OBJDIR)/plot1mmx.o $(OBJDIR)/gscale_blk.o 
$(OBJDIR)/gfticl.o : gfticl.for 
$(OBJDIR)/uinitg.o : uinitg.for 
$(OBJDIR)/grfeby.o : grfeby.for 
$(OBJDIR)/putchr.o : putchr.for $(OBJDIR)/plot1mmx.o 
$(OBJDIR)/mulcdrb.o : mulcdrb.for $(OBJDIR)/plot1mmx.o $(OBJDIR)/mulfdr_blk.o $(OBJDIR)/bottoms_up.o 
$(OBJDIR)/gaxchk.o : gaxchk.for $(OBJDIR)/plot1mmx.o $(OBJDIR)/gscale_blk.o $(OBJDIR)/gaxes_blk.o 
$(OBJDIR)/grf3op.o : grf3op.for 
$(OBJDIR)/gaxset.o : gaxset.for $(OBJDIR)/gaxes_blk.o 
$(OBJDIR)/gscrsc.o : gscrsc.for $(OBJDIR)/gscale_blk.o $(OBJDIR)/plot1mmx.o 
$(OBJDIR)/gaxspc.o : gaxspc.for $(OBJDIR)/gaxes_blk.o 
$(OBJDIR)/gftics.o : gftics.for $(OBJDIR)/gftic.o 
$(OBJDIR)/grafc3.o : grafc3.for $(OBJDIR)/plot1mmx.o $(OBJDIR)/gscale_blk.o 
$(OBJDIR)/grfasym2.o : grfasym2.for $(OBJDIR)/plot1mmx.o $(OBJDIR)/gscale_blk.o 
$(OBJDIR)/grafc2.o : grafc2.for $(OBJDIR)/plot1mmx.o $(OBJDIR)/gscale_blk.o 
$(OBJDIR)/gmmselb.o : gmmselb.for 
$(OBJDIR)/gfticj.o : gfticj.for 
$(OBJDIR)/gscini.o : gscini.for $(OBJDIR)/gscale_blk.o 
$(OBJDIR)/grfebx.o : grfebx.for 
$(OBJDIR)/gfctrs.o : gfctrs.for 
$(OBJDIR)/grsflux.o : grsflux.for $(OBJDIR)/plot1mmx.o $(OBJDIR)/gscale_blk.o 
$(OBJDIR)/gaxes0.o : gaxes0.for $(OBJDIR)/gaxes_blk.o 
$(OBJDIR)/gftici.o : gftici.for $(OBJDIR)/gftic.o 
$(OBJDIR)/grf3f1.o : grf3f1.for 
$(OBJDIR)/gscydf.o : gscydf.for $(OBJDIR)/plot1mmx.o $(OBJDIR)/gscale_blk.o $(OBJDIR)/gaxes_blk.o 
$(OBJDIR)/grafwc.o : grafwc.for $(OBJDIR)/plot1mmx.o $(OBJDIR)/gscale_blk.o 
$(OBJDIR)/gfpaks.o : gfpaks.for 
$(OBJDIR)/gftcls.o : gftcls.for $(OBJDIR)/gftic.o 
$(OBJDIR)/grfmf1.o : grfmf1.for $(OBJDIR)/plot1mmx.o 
$(OBJDIR)/wkchk.o : wkchk.for 
$(OBJDIR)/gscsm0.o : gscsm0.for $(OBJDIR)/gscale_blk.o 
$(OBJDIR)/dplotk.o : dplotk.for $(OBJDIR)/gftic.o 
$(OBJDIR)/fnexc.o : fnexc.for 
$(OBJDIR)/gscxdf.o : gscxdf.for $(OBJDIR)/plot1mmx.o $(OBJDIR)/gscale_blk.o $(OBJDIR)/gaxes_blk.o 
$(OBJDIR)/put1f2.o : put1f2.for 
$(OBJDIR)/gradum.o : gradum.for 
$(OBJDIR)/condm0.o : condm0.for 
$(OBJDIR)/grf3.o : grf3.for 
$(OBJDIR)/conplt.o : conplt.for $(OBJDIR)/plot1mmx.o 
$(OBJDIR)/grf4f1.o : grf4f1.for 
$(OBJDIR)/r8_trangr.o : r8_trangr.for 
$(OBJDIR)/gpreset.o : gpreset.for 
$(OBJDIR)/gaxgdf.o : gaxgdf.for $(OBJDIR)/gaxes_blk.o 
$(OBJDIR)/dplot1.o : dplot1.for $(OBJDIR)/plot1mmx.o $(OBJDIR)/plot1mmx.o $(OBJDIR)/gftic.o $(OBJDIR)/bottoms_up.o 
$(OBJDIR)/zfscc1.o : zfscc1.for 
$(OBJDIR)/lmvrti.o : lmvrti.for 
$(OBJDIR)/gralbl.o : gralbl.for $(OBJDIR)/bottoms_up.o 
$(OBJDIR)/gftcss.o : gftcss.for $(OBJDIR)/gftic.o 
$(OBJDIR)/limplotr.o : limplotr.for 
$(OBJDIR)/grffit.o : grffit.for 
$(OBJDIR)/grafx3.o : grafx3.for $(OBJDIR)/gscale_blk.o $(OBJDIR)/plot1mmx.o 
$(OBJDIR)/grfnex.o : grfnex.for 
$(OBJDIR)/g3inx1.o : g3inx1.for 
$(OBJDIR)/initvis.o : initvis.for 
$(OBJDIR)/gscset.o : gscset.for $(OBJDIR)/plot1mmx.o $(OBJDIR)/gscale_blk.o $(OBJDIR)/gaxes_blk.o 
$(OBJDIR)/ixmmx.o : ixmmx.for $(OBJDIR)/plot1mmx.o 
$(OBJDIR)/uf_sincos.o : uf_sincos.for 
$(OBJDIR)/gscsv0.o : gscsv0.for $(OBJDIR)/gscale_blk.o 
$(OBJDIR)/gscrst.o : gscrst.for $(OBJDIR)/plot1mmx.o $(OBJDIR)/gscale_blk.o $(OBJDIR)/gaxes_blk.o 
$(OBJDIR)/dplotm.o : dplotm.for $(OBJDIR)/gftic.o 
$(OBJDIR)/grafxf.o : grafxf.for $(OBJDIR)/plot1mmx.o $(OBJDIR)/gscale_blk.o 
$(OBJDIR)/ufmtsel.o : ufmtsel.for 
$(OBJDIR)/get1f2.o : get1f2.for 
$(OBJDIR)/grfah0.o : grfah0.for 
$(OBJDIR)/conctr.o : conctr.for 
$(OBJDIR)/isfnds.o : isfnds.for $(OBJDIR)/plot1mmx.o 
$(OBJDIR)/dplnst.o : dplnst.for $(OBJDIR)/gftic.o 
$(OBJDIR)/g3msel.o : g3msel.for 
$(OBJDIR)/mulcdr.o : mulcdr.for $(OBJDIR)/plot1mmx.o $(OBJDIR)/mulfdr_blk.o $(OBJDIR)/bottoms_up.o 
$(OBJDIR)/concts.o : concts.for 
$(OBJDIR)/grfmm1.o : grfmm1.for $(OBJDIR)/plot1mmx.o 
$(OBJDIR)/r8_grf4f1.o : r8_grf4f1.for 
$(OBJDIR)/grfasym.o : grfasym.for $(OBJDIR)/plot1mmx.o $(OBJDIR)/gscale_blk.o 
$(OBJDIR)/graopt.o : graopt.for $(OBJDIR)/plot1mmx.o 
$(OBJDIR)/ilval.o : ilval.for 
$(OBJDIR)/grafx4.o : grafx4.for $(OBJDIR)/plot1mmx.o $(OBJDIR)/gscale_blk.o 
$(OBJDIR)/grfuf1.o : grfuf1.for 
$(OBJDIR)/grfssym.o : grfssym.for $(OBJDIR)/plot1mmx.o $(OBJDIR)/gscale_blk.o 
$(OBJDIR)/grfmbc.o : grfmbc.for 
$(OBJDIR)/get1ff.o : get1ff.for 
$(OBJDIR)/mlscck.o : mlscck.for 
$(OBJDIR)/dpsyst.o : dpsyst.for $(OBJDIR)/gftic.o 
$(OBJDIR)/gscspc.o : gscspc.for $(OBJDIR)/gscale_blk.o 
