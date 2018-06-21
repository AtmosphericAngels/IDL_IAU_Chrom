FUNCTION conc_date, date1, date2, cdate1=cdate1, cdate2=cdate2

  caldat, date1, mm,dd,yy
  cdate1=string(yy,format='(I4)')+string(mm,format='(I02)')+string(dd,format='(I02)')
  caldat, date2, mm,dd,yy,hh,mn,ss
  cdate2=string(yy,format='(I4)')+string(mm,format='(I02)')+string(dd,format='(I02)') $ ; +'_'
    +string(hh,format='(I02)')+string(mn,format='(I02)')

  cdate = cdate1+'_'+cdate2

  RETURN, cdate

END