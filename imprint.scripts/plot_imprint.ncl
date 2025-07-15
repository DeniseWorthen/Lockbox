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
   rtsrc = RT+"imprint/"
   tlatname="TLAT"
   tlonname="TLON"

   fname = "iceh_inst."+cdate+".stddev.nc"
   icef = addfile(rtsrc+fname,"r")
;************************************************
; ice
;************************************************

  lstep=1-1
  print("cdate = "+cdate)

  ; ice field, stddev
  varname="aice_h"
  wgt = 1.0e3
  levs = (/0.0, 1.0, 0.02/)
  icevar = icef->$varname$(lstep,:,:)
  icevar = icevar*wgt

  icevar@lon2d=icef->$tlonname$
  icevar@lat2d=icef->$tlatname$
  ;printVarSummary(icevar)
  icevar@units=""
  icevar@long_name=""

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
  wkstype@wkWidth=800
  wkstype@wkHeight=800
  wks   = gsn_open_wks (wkstype,"cice5")              ; send graphics to X11 window

  ;gsn_define_colormap(wks,"MPL_Blues")
  ;gsn_reverse_colormap(wks)

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
    res@mpMaxLatF            =  -30.0
    res@mpShapeMode = "FreeAspect"
     ;size = 3.00
     ;res@vpWidthF = size/2
     ;res@vpHeightF = size
    ;res@mpMinLonF            =  -90
    ;res@mpMaxLonF            =  90.
  end if

  res@cnFillDrawOrder   = "PreDraw"             ; draw contours first
  res@cnFillOn          = True                  ; color fill
  res@cnLinesOn         = False                 ; no contour lines
  res@cnLineLabelsOn    = False
  res@lbLabelBarOn      = True
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
  res@tiMainString = rtsrc+" "+cdate+" "+varname
  res@tiMainFontHeightF = .01

  ;res@gsnCenterStringOrthogonalPosF = .15	    ; raise the CenterString slightly
  ;res@gsnStringFontHeightF = 0.01
  ;res@gsnCenterString = cd_string(times(lstep),"%d %F")

;************************************************
; Resources for the polyline
;************************************************

  res@mpGreatCircleLinesOn = True
  pres                  = True           ; polyline mods desired
  pres@gsLineThicknessF = 2.0            ; line thickness
  pres@gsLineColor      = "black"         ; color of lines
  pres@gsLineDashPattern =  1            ; dashes
  ;pres@tfPolyDrawOrder = "postDraw"

;************************************************
;
;************************************************

  if(polar .eq. "T")then
    plot = gsn_csm_contour_map_polar(wks,icevar,res)    ; create the plot
  else
    plot = gsn_csm_contour_map(wks,icevar,res)    ; create the plot
  end if

  ;dum= gsn_add_polyline(wks,plot,(/lon1, lon2/) , (/lat1, lat2/) ,pres)
  ;pum= gsn_add_polymarker(wks,plot,center_xlon,center_xlat,mkres)

  draw(plot)
  frame(wks)

exit

end
