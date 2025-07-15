;************************************************
; These files are loaded by default in NCL V6.2.0 and newer
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
; load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"

  load "$NCARG_ROOT/lib/ncarg/nclscripts/contrib/cd_string.ncl"
;************************************************
begin

  polar = "F"
  ;hemi="NH"
  hemi="SH"

  model = "m6c5"
  minlat = 66.0
  ;minlat = 76.0
  ;minlat = 88.0
  ;minlat = 80.0

  ;minlat =  88.0
  maxlat = -50.0
  ;minlon =  140.0
  ;maxlon =  270.0

  RT = "/gpfs/f6/drsa-hurr1/scratch/Denise.Worthen/"

;************************************************
; read in netCDF file
;************************************************

   cyear = "2020"
    cmon = "11"
    cday = "01"
    secs = "21600"
   cdate = cyear+"-"+cmon+"-"+cday+"-"+secs
   tlatname="TLAT"
   tlonname="TLON"

   rts = (/"imprint/base/", "imprint/bilin/" /)
   plotlabs = (/"Base", "Bilin2ice" /)
   nplots = dimsizes(rts)

;************************************************
; ice
;************************************************

  lstep=1-1
  print("cdate = "+cdate)

  ; ice field, stddev
  varname="aice_h"
  wgt = 1.0e3
  ;levs = (/0.0, 1.0, 0.02/)
  levs = (/0.0, 0.50, 0.01/)
  fnames = RT+rts+varname+"."+cdate+".stddev.nc"
  print(fnames)
  files = addfiles(fnames,"r")

;************************************************
; add a poly line showing the polar seem along
; great circle path from (lon1,lat1) to (lon2,lat2)
;************************************************

  lon1=  60.00
  lon2=-120.00
  lat1 = 60.00
  lat2 = 60.00

;************************************************
; define the map
;************************************************
  wkstype="X11"
  ;wkstype="png"
  wkstitle="imprint"
  wkstype@wkWidth=800
  wkstype@wkHeight=800
  wks   = gsn_open_wks (wkstype,wkstitle)              ; send graphics to X11 window

  plot = new(nplots,graphic)
  res                      = True        ; plot mods desired
  res@gsnDraw              = False       ; do not draw until the end
  res@gsnFrame             = False       ; do not automatically advance 'frame'

  ;res@cnFillMode = "MeshFill"
  res@cnFillMode = "RasterFill"
  res@mpFillOn   = True
  ;res@mpOutlineOn   = False

  res@gsnStringFontHeightF = 0.05

  if(polar .eq. "T")then
    res@gsnPolar   =  hemi                         ; specify the hemisphere
    if(hemi .eq. "NH")then
     res@mpMinLatF    =  minlat                          ; minimum lat to plot
     res@mpCenterLonF = -45
    end if
    if(hemi .eq. "SH")then
     res@mpMaxLatF    = -50                            ; minimum lat to plot
     res@mpCenterLonF = 0.0
    end if
  else
    res@vpWidthF          = 0.70
    res@vpHeightF         = 0.50
    ;res@mpMinLatF            =  -80      ; range to zoom in on
    res@mpMaxLatF            =  -50.0
    res@mpShapeMode = "FreeAspect"
     ;size = 3.00
     ;res@vpWidthF = size/2
     ;res@vpHeightF = 1.5
    res@mpMinLonF            =  150
    res@mpMaxLonF            =  350.
    res@mpCenterLonF = 240.
  end if

  res@cnFillDrawOrder   = "PreDraw"             ; draw contours first
  res@cnFillOn          = True                  ; color fill
  res@cnLinesOn         = False                 ; no contour lines
  res@cnLineLabelsOn    = False
  res@lbLabelBarOn      = False
  res@lbBoxLinesOn      = False                   ; Turn off labelbar box lines
  ;res@cnFillOn          = False                  ; color fill

  res@cnLevelSelectionMode = "ManualLevels"     ; set the contour levels with the following 3 resources
  res@cnMinLevelValF  = levs(0)                      ; set the minimum contour level
  res@cnMaxLevelValF  = levs(1)                      ; set the maximum contour level
  res@cnLevelSpacingF = levs(2)                      ; set the interval between contours

  ; res@cnLevelSelectionMode        = "ExplicitLevels"
  ; res@cnLevels                    = (/     0.15,0.50,0.7,0.8,0.85,0.9,0.950/)
  ; res@lbLabelStrings              = (/0.00,0.15,0.50,0.7,0.8,0.85,0.9,0.950/)+""

  ;res@cnFillPalette        = "MPL_rainbow"              ; choose colormap
  ;res@cnFillPalette        = "BlWhRe"              ; choose colormap
  ;res@cnFillPalette        = "BlueWhiteOrangeRed"              ; choose colormap
  ;res@cnFillPalette        = "NCV_roullet"              ; choose colormap
  res@cnFillPalette        = "NCV_banded"              ; choose colormap

  ;res@tiMainString = dirname+" "+cday
 ; res@tiMainString = rtsrc+" "+cdate+" "+varname
  res@tiMainFontHeightF = .01

  ;res@gsnCenterStringOrthogonalPosF = .15	    ; raise the CenterString slightly
  ;res@gsnStringFontHeightF = 0.01
  ;res@gsnCenterString = cd_string(times(lstep),"%d %F")

; ***************************************************
; panel resources
; **************************************************

  resP                       = True     ; modify the panel plot
  resP@gsnMaximize           = True     ; maximize in frame
  resP@gsnFrame              = False    ; don't advance frame yet
  ;resP@gsnPanelYWhiteSpacePercent = 5
  resP@gsnPanelYWhiteSpacePercent = 5
  resP@gsnPanelLabelBar      = True     ; turn on a common labelbar for the entire panel plot

  resP@gsnPanelMainFontHeightF = 0.015  ; new resource added in NCL V6.4.0
  resP@gsnPanelMainString = "C96 mx100 10-member StdDev "+cdate+"  "+varname
  resP@gsnMaximize        = True        ; use full page

  resP@gsnPanelFigureStrings = (/plotlabs(0),plotlabs(1)/)
  resP@amJust = "TopLeft"
  resP@gsnPanelFigureStringsFontHeightF = 0.010

;************************************************
;
;************************************************

  do ii = 0,nplots-1
   icevar = files[ii]->$varname$(0,:,:)
   ;printVarSummary(icevar)
   icevar@units=""
   icevar@long_name=""

   icevar = icevar*wgt
   icevar@lon2d=files[ii]->$tlonname$
   icevar@lat2d=files[ii]->$tlatname$

    if(polar .eq. "T")then
     plot(ii) = gsn_csm_contour_map_polar(wks,icevar,res)    ; create the plot
    else
     plot(ii) = gsn_csm_contour_map(wks,icevar,res)    ; create the plot
    end if
  end do

;************************************************
; make the panel
;************************************************

   gsn_panel(wks,plot,(/nplots,1/),resP)
   frame(wks)

;************************************************
;
;************************************************

  if(wkstype .eq. "png")then
   imcmd = "convert -trim "+wkstitle+".png "+wkstitle+".jpg"
   system(imcmd)
  end if

exit
end
