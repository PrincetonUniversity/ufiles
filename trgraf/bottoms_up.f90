module bottoms_up

  ! trgraf-local module to control raising of lower portion of graph
  ! display to make room for an extra one or two lines of labels

  ! bottoms_up_sub.f90 contains routine for accessing this module data

  integer, parameter :: idybu = 23  ! raster count per line

  integer :: ibrk_lbl3 = 0  ! break point in "lbl3" passed to some graphs 

  integer :: ibu_raise = 0  ! #text lines by which to raise bottom portion

end module bottoms_up
