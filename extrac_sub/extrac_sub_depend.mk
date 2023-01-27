$(OBJDIR)/extrac_mod.o : extrac_mod.f90 
$(OBJDIR)/expick.o : expick.for $(OBJDIR)/extrac_mod.o 
$(OBJDIR)/exblkd.o : exblkd.for $(OBJDIR)/extrac_mod.o 
$(OBJDIR)/exblok.o : exblok.for $(OBJDIR)/extrac_mod.o 
$(OBJDIR)/expget.o : expget.for 
$(OBJDIR)/extrk1.o : extrk1.for $(OBJDIR)/extrac_mod.o 
$(OBJDIR)/exched.o : exched.for 
$(OBJDIR)/exsclb.o : exsclb.for $(OBJDIR)/extrac_mod.o 
$(OBJDIR)/extrk2.o : extrk2.for $(OBJDIR)/extrac_mod.o 
$(OBJDIR)/expcki.o : expcki.for 
$(OBJDIR)/exintx.o : exintx.for $(OBJDIR)/extrac_mod.o 
$(OBJDIR)/expwrt.o : expwrt.for $(OBJDIR)/extrac_mod.o 
$(OBJDIR)/exint0.o : exint0.for $(OBJDIR)/extrac_mod.o 
$(OBJDIR)/expmck.o : expmck.for 
$(OBJDIR)/expint.o : expint.for 
$(OBJDIR)/experr.o : experr.for 
