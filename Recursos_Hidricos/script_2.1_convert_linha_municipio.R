	
	p1_45_ana<-st_intersection(rec_p0,p1_45_cat_ana) %>% st_drop_geometry()
	p1_45_085<-st_intersection(rec_p0,p1_45_cat_085) %>% st_drop_geometry()
	p1_45_FUL<-st_intersection(rec_p0,p1_45_cat_FUL) %>% st_drop_geometry()

	p2_45_ana<-st_intersection(rec_p0,p2_45_cat_ana) %>% st_drop_geometry()
	p2_45_085<-st_intersection(rec_p0,p2_45_cat_085) %>% st_drop_geometry()
	p2_45_FUL<-st_intersection(rec_p0,p2_45_cat_FUL) %>% st_drop_geometry()

	p3_45_ana<-st_intersection(rec_p0,p3_45_cat_ana) %>% st_drop_geometry()
	p3_45_085<-st_intersection(rec_p0,p3_45_cat_085) %>% st_drop_geometry()
	p3_45_FUL<-st_intersection(rec_p0,p3_45_cat_FUL) %>% st_drop_geometry()

	p1_85_ana<-st_intersection(rec_p0,p1_85_cat_ana) %>% st_drop_geometry()
	p1_85_085<-st_intersection(rec_p0,p1_85_cat_085) %>% st_drop_geometry()
	p1_85_FUL<-st_intersection(rec_p0,p1_85_cat_FUL) %>% st_drop_geometry()

	p2_85_ana<-st_intersection(rec_p0,p2_85_cat_ana) %>% st_drop_geometry()
	p2_85_085<-st_intersection(rec_p0,p2_85_cat_085) %>% st_drop_geometry()
	p2_85_FUL<-st_intersection(rec_p0,p2_85_cat_FUL) %>% st_drop_geometry()

	p3_85_ana<-st_intersection(rec_p0,p3_85_cat_ana) %>% st_drop_geometry()
	p3_85_085<-st_intersection(rec_p0,p3_85_cat_085) %>% st_drop_geometry()
	p3_85_FUL<-st_intersection(rec_p0,p3_85_cat_FUL) %>% st_drop_geometry()

	rec_p1_45_ana<-rec_df
	rec_p1_45_ana$GFDL_ESM4<-NA  ; rec_p1_45_ana$INM_CM5<-NA    ; rec_p1_45_ana$MPI_ESM1_2_HR<-NA
	rec_p1_45_ana$MRI_ESM2_0<-NA ; rec_p1_45_ana$NORESM2_MM<-NA ; rec_p1_45_ana$OBS<-NA

	rec_p1_45_085<-rec_df
	rec_p1_45_085$GFDL_ESM4<-NA  ; rec_p1_45_085$INM_CM5<-NA    ; rec_p1_45_085$MPI_ESM1_2_HR<-NA
	rec_p1_45_085$MRI_ESM2_0<-NA ; rec_p1_45_085$NORESM2_MM<-NA ; rec_p1_45_085$OBS<-NA

	rec_p1_45_FUL<-rec_df
	rec_p1_45_FUL$GFDL_ESM4<-NA  ; rec_p1_45_FUL$INM_CM5<-NA    ; rec_p1_45_FUL$MPI_ESM1_2_HR<-NA
	rec_p1_45_FUL$MRI_ESM2_0<-NA ; rec_p1_45_FUL$NORESM2_MM<-NA ; rec_p1_45_FUL$OBS<-NA

	rec_p2_45_ana<-rec_df
	rec_p2_45_ana$GFDL_ESM4<-NA  ; rec_p2_45_ana$INM_CM5<-NA    ; rec_p2_45_ana$MPI_ESM1_2_HR<-NA
	rec_p2_45_ana$MRI_ESM2_0<-NA ; rec_p2_45_ana$NORESM2_MM<-NA ; rec_p2_45_ana$OBS<-NA

	rec_p2_45_085<-rec_df
	rec_p2_45_085$GFDL_ESM4<-NA  ; rec_p2_45_085$INM_CM5<-NA    ; rec_p2_45_085$MPI_ESM1_2_HR<-NA
	rec_p2_45_085$MRI_ESM2_0<-NA ; rec_p2_45_085$NORESM2_MM<-NA ; rec_p2_45_085$OBS<-NA

	rec_p2_45_FUL<-rec_df
	rec_p2_45_FUL$GFDL_ESM4<-NA  ; rec_p2_45_FUL$INM_CM5<-NA    ; rec_p2_45_FUL$MPI_ESM1_2_HR<-NA
	rec_p2_45_FUL$MRI_ESM2_0<-NA ; rec_p2_45_FUL$NORESM2_MM<-NA ; rec_p2_45_FUL$OBS<-NA

	rec_p3_45_ana<-rec_df
	rec_p3_45_ana$GFDL_ESM4<-NA  ; rec_p3_45_ana$INM_CM5<-NA    ; rec_p3_45_ana$MPI_ESM1_2_HR<-NA
	rec_p3_45_ana$MRI_ESM2_0<-NA ; rec_p3_45_ana$NORESM2_MM<-NA ; rec_p3_45_ana$OBS<-NA

	rec_p3_45_085<-rec_df
	rec_p3_45_085$GFDL_ESM4<-NA  ; rec_p3_45_085$INM_CM5<-NA    ; rec_p3_45_085$MPI_ESM1_2_HR<-NA
	rec_p3_45_085$MRI_ESM2_0<-NA ; rec_p3_45_085$NORESM2_MM<-NA ; rec_p3_45_085$OBS<-NA

	rec_p3_45_FUL<-rec_df
	rec_p3_45_FUL$GFDL_ESM4<-NA  ; rec_p3_45_FUL$INM_CM5<-NA    ; rec_p3_45_FUL$MPI_ESM1_2_HR<-NA
	rec_p3_45_FUL$MRI_ESM2_0<-NA ; rec_p3_45_FUL$NORESM2_MM<-NA ; rec_p3_45_FUL$OBS<-NA

	rec_p1_85_ana<-rec_df
	rec_p1_85_ana$GFDL_ESM4<-NA  ; rec_p1_85_ana$INM_CM5<-NA    ; rec_p1_85_ana$MPI_ESM1_2_HR<-NA
	rec_p1_85_ana$MRI_ESM2_0<-NA ; rec_p1_85_ana$NORESM2_MM<-NA ; rec_p1_85_ana$OBS<-NA

	rec_p1_85_085<-rec_df
	rec_p1_85_085$GFDL_ESM4<-NA  ; rec_p1_85_085$INM_CM5<-NA    ; rec_p1_85_085$MPI_ESM1_2_HR<-NA
	rec_p1_85_085$MRI_ESM2_0<-NA ; rec_p1_85_085$NORESM2_MM<-NA ; rec_p1_85_085$OBS<-NA

	rec_p1_85_FUL<-rec_df
	rec_p1_85_FUL$GFDL_ESM4<-NA  ; rec_p1_85_FUL$INM_CM5<-NA    ; rec_p1_85_FUL$MPI_ESM1_2_HR<-NA
	rec_p1_85_FUL$MRI_ESM2_0<-NA ; rec_p1_85_FUL$NORESM2_MM<-NA ; rec_p1_85_FUL$OBS<-NA

	rec_p2_85_ana<-rec_df
	rec_p2_85_ana$GFDL_ESM4<-NA  ; rec_p2_85_ana$INM_CM5<-NA    ; rec_p2_85_ana$MPI_ESM1_2_HR<-NA
	rec_p2_85_ana$MRI_ESM2_0<-NA ; rec_p2_85_ana$NORESM2_MM<-NA ; rec_p2_85_ana$OBS<-NA

	rec_p2_85_085<-rec_df
	rec_p2_85_085$GFDL_ESM4<-NA  ; rec_p2_85_085$INM_CM5<-NA    ; rec_p2_85_085$MPI_ESM1_2_HR<-NA
	rec_p2_85_085$MRI_ESM2_0<-NA ; rec_p2_85_085$NORESM2_MM<-NA ; rec_p2_85_085$OBS<-NA

	rec_p2_85_FUL<-rec_df
	rec_p2_85_FUL$GFDL_ESM4<-NA  ; rec_p2_85_FUL$INM_CM5<-NA    ; rec_p2_85_FUL$MPI_ESM1_2_HR<-NA
	rec_p2_85_FUL$MRI_ESM2_0<-NA ; rec_p2_85_FUL$NORESM2_MM<-NA ; rec_p2_85_FUL$OBS<-NA

	rec_p3_85_ana<-rec_df
	rec_p3_85_ana$GFDL_ESM4<-NA  ; rec_p3_85_ana$INM_CM5<-NA    ; rec_p3_85_ana$MPI_ESM1_2_HR<-NA
	rec_p3_85_ana$MRI_ESM2_0<-NA ; rec_p3_85_ana$NORESM2_MM<-NA ; rec_p3_85_ana$OBS<-NA

	rec_p3_85_085<-rec_df
	rec_p3_85_085$GFDL_ESM4<-NA  ; rec_p3_85_085$INM_CM5<-NA    ; rec_p3_85_085$MPI_ESM1_2_HR<-NA
	rec_p3_85_085$MRI_ESM2_0<-NA ; rec_p3_85_085$NORESM2_MM<-NA ; rec_p3_85_085$OBS<-NA

	rec_p3_85_FUL<-rec_df
	rec_p3_85_FUL$GFDL_ESM4<-NA  ; rec_p3_85_FUL$INM_CM5<-NA    ; rec_p3_85_FUL$MPI_ESM1_2_HR<-NA
	rec_p3_85_FUL$MRI_ESM2_0<-NA ; rec_p3_85_FUL$NORESM2_MM<-NA ; rec_p3_85_FUL$OBS<-NA

