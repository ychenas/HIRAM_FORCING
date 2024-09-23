#!user/bin/evn Rscript

#Hello this just to have a fun in GitHUB

#=========================================================================
#YCHEN scripts 20140330 at  LSCE/IPSL
#Script for reading cru dataset after zooming
#then put it to the land compress format as the forcing file for ORCHIDEE  
#=========================================================================
#forcing directory
#setwd("/home/users/ychen/R/Rscripts/myforcing/cru_test")
#james/julian regrided floder
#=========================================================================
#setwd("/home/orchidee02/jryder/CRU_regridded/")
#setwd("/home/orchidee03/jotto/regridding_cru/")

#input_nc_varnames<-c("lon", "lat", "time",
#	"Tair","LWdown","SWdown","WindE","WindN","Snowf",
#	"Rainf","PSurf","Qair")

#fun_compress <- function(arg1=c("atmos_4xdaily_lm_20070101.nc"),
#			arg2=c("hiram_20070101_compress.nc"))
#{      
for (iyr in 2008) {
for (imon in 1:12) { 
print(paste("This script starts the job on: ", Sys.time(),sep="")) 
#set the start date
st_yy <- iyr
st_mm <- imon
st_dd <- 01
st_hh <- 00

date_name <- paste( formatC(st_yy,width=4,flag="0"),
                    formatC(st_mm,width=2,flag="0"),
                    formatC(st_dd,width=2,flag="0"),sep="")

#======== ptintout the argunments: input &  ouput file location ==========
arg1=c(paste("/work/dadm/c384_amip_new/combined/",date_name,"/atmos_4xdaily_lm_",date_name,".nc",sep=""))
arg2=c(paste("hiram_",date_name,"_compress.nc",sep=""))

print(paste("arg1: for input file location & name ;",  arg1)) 
print(paste("arg2: for output file location & name ;", arg2))

#======= initial seting for function =============
in_filename  <- arg1
out_filename <- arg2

time_origin <- paste( formatC(st_yy,width=4,flag="0"),"-",
                      formatC(st_mm,width=2,flag="0"),"-",
                      formatC(st_dd,width=2,flag="0")," ",
                      formatC(st_hh,width=2,flag="0"),":00:00",sep="")
time_since  <- time_origin
#==================================================
source("/work/ychen/scripts/R/function/src_function_ychen.R")
# get lon from 
land_input <- fun_read_nc(arg1="/data3/dadm/HiRAM_data/hiram_c384_land_mask.nc")

lon <-  (land_input$grid_xt)  

lon <- c(lon[(1536/2+1):1536]-360,lon[1:(1536/2)])
#lon[lon>180] <- lon - 360 
lat <-  land_input$grid_yt
mask <- land_input$land_mask
temp<-mask
mask[1:(1536/2),]  <- temp[(1536/2+1):1536,]
mask[(1536/2+1):1536,] <- temp[1:(1536/2),] 

print("load libraries...")
library(ncdf)

#setting the input_information "from in_filename" & "time_orgin" 
input_nc  <- open.ncdf(in_filename)
print(paste("Reading the input file:", in_filename))


#======= import dimesions ====================================
# Import  cru-zoom grid data-set from input_nc file
#============================================================= 
d1 <- lon #get.var.ncdf(input_nc,"grid_xt",start=1,count=read_nx)
read_nx <- length(d1)
d2 <- lat #get.var.ncdf(input_nc,"grid_yt",start=1,count=read_ny)
read_ny  <-  length(d2)
d3 <- get.var.ncdf(input_nc,"time")
read_nt <- length(d3)   
print("Read_nc_Dimension OK")

#import variables
v1  <- get.var.ncdf(input_nc,"t_surf", start=c(1,1,1), count=c(read_nx,read_ny,read_nt))
temp<-v1
v1[1:(1536/2),,]  <- temp[(1536/2+1):1536,,]
v1[(1536/2+1):1536,,] <- temp[1:(1536/2),,] 

print("Tair OK")
v2  <- get.var.ncdf(input_nc,"lwdn_sfc", start=c(1,1,1), count=c(read_nx,read_ny,read_nt))
temp<-v2
v2[1:(1536/2),,]  <- temp[(1536/2+1):1536,,]
v2[(1536/2+1):1536,,] <- temp[1:(1536/2),,]

print("LWdown OK")
v3  <- get.var.ncdf(input_nc,"swdn_sfc", start=c(1,1,1), count=c(read_nx,read_ny,read_nt))
temp<-v3
v3[1:(1536/2),,]  <- temp[(1536/2+1):1536,,]
v3[(1536/2+1):1536,,] <- temp[1:(1536/2),,]

print("SWdown OK")
v4  <- get.var.ncdf(input_nc,"us", start=c(1,1,1), count=c(read_nx,read_ny,read_nt))
temp<-v4
v4[1:(1536/2),,]  <- temp[(1536/2+1):1536,,]
v4[(1536/2+1):1536,,] <- temp[1:(1536/2),,]

v5  <- get.var.ncdf(input_nc,"vs", start=c(1,1,1), count=c(read_nx,read_ny,read_nt))
temp<-v5
v5[1:(1536/2),,]  <- temp[(1536/2+1):1536,,]
v5[(1536/2+1):1536,,] <- temp[1:(1536/2),,]


print("WindE/N OK")
v6  <- get.var.ncdf(input_nc,"snow_tot", start=c(1,1,1), count=c(read_nx,read_ny,read_nt))
temp<-v6
v6[1:(1536/2),,]  <- temp[(1536/2+1):1536,,]
v6[(1536/2+1):1536,,] <- temp[1:(1536/2),,]

print("Snowf OK")
v7  <- get.var.ncdf(input_nc,"precip", start=c(1,1,1), count=c(read_nx,read_ny,read_nt))
temp<-v7
v7[1:(1536/2),,]  <- temp[(1536/2+1):1536,,]
v7[(1536/2+1):1536,,] <- temp[1:(1536/2),,]


print("Rainf OK")
v8  <- get.var.ncdf(input_nc,"slp", start=c(1,1,1), count=c(read_nx,read_ny,read_nt))
temp<-v8
v8[1:(1536/2),,]  <- temp[(1536/2+1):1536,,]
v8[(1536/2+1):1536,,] <- temp[1:(1536/2),,]

print("PSurf OK")
v9  <- get.var.ncdf(input_nc,"q_ref", start=c(1,1,1), count=c(read_nx,read_ny,read_nt))
temp<-v9
v9[1:(1536/2),,]  <- temp[(1536/2+1):1536,,]
v9[(1536/2+1):1536,,] <- temp[1:(1536/2),,]

print("Qair OK")


# get number of time step and time step in seconds
ntstep <- length(d3)
print(paste("time steps:",ntstep,sep=""))
time_start_steps  <- NULL
time_start_second <- NULL 
for (i  in  1:ntstep) 
{
	time_start_steps[i]  <- i
	time_start_second[i] <- i*3600*6 
} 
#==================================================================

#========Start compressing Procedure ==============================	
# start two counter for the land_id  and exact sequency of matrix
#  cont1:
#  for example  a 3 by 3 array the exact secquency of each point is 
#  1 2 3
#  4 5 6
#  7 8 9
#  cont2:
#  for eaxmple with 3 by 3 array only have 3 point at certain exact point 
#  0 0 1
#  0 2 0
#  0 0 3    
#so we need to create a funtion land() for the mapping
#land(1) = 3
#land(2) = 5
#land(3) = 9
#==================================================================

aa<-NULL
cont1  <- 0  # for exact sequency of array  
cont2  <- 0  # for land_id
land   <- NULL 
nx     <- length(d1)  #get x dimension
ny     <- length(d2)  #get y dimension
mask_id_arr <- array(0,dim=c(nx,ny))
for ( jj in 1:ny ) {
for ( ii in 1:nx )  { 
            cont1 <- cont1 + 1
            if (  mask[ii,jj] > 0.0 ) {
            cont2 <- cont2+1
            #aa[cont2]<-v1[ii,jj,1] dummy variable use to cheack mapping 
            land[cont2] <- cont1    # sequencial with the all index  
            mask_id_arr[ii,jj] <- cont2  # sequential index by land
	    # print(paste("x:", ii, "y:",jj)) 
            } # for land mass
} # for longitude
} # for latitude


print(paste("totoal lan points:", length(land),sep=""))
#=copy land to dimension 4: d4==== 
d4 <- land 
#==================================


print("finished the coordinate information compressing.")


#======= Satrt to creat the forcing array with compressed format (time, land)========
nland <- length(d4) # get numbers(dimension) of d4
mv    <- 1e+20 #set missing value
#======= INITAIL THE DATA ARRAY
zoom_swdown   <-  array ( 0, dim = c(nland, ntstep) ) 
zoom_lwdown   <-  array ( 0, dim = c(nland, ntstep) ) 
zoom_rainfall <-  array ( 0, dim = c(nland, ntstep) ) 
zoom_snowfall <-  array ( 0, dim = c(nland, ntstep) ) 
zoom_qair     <-  array ( 0, dim = c(nland, ntstep) ) 
zoom_psurf    <-  array ( 0, dim = c(nland, ntstep) ) 
zoom_tair     <-  array ( 0, dim = c(nland, ntstep) ) 
zoom_wind_e   <-  array ( 0, dim = c(nland, ntstep) ) 
zoom_wind_n   <-  array ( 0, dim = c(nland, ntstep) )
zoom_nav_lon  <-  array ( 0, dim = c(nx, ny) ) 
zoom_nav_lat  <-  array ( 0, dim = c(nx, ny) ) 
#======= create array for nav_lon & nav_lat for output nc-file ======================= 
for ( i in 1:nx) {   #nx=129
   for ( j in 1:ny) {   #ny=119
        zoom_nav_lon[ i, j] <-  d1[i] 
	zoom_nav_lat[ i, j] <-  d2[j]  #d2[ny-j+1]    #reverse the latitude index
     #print(paste("nav_lon",zoom_nav_lon[i,j],"i:",i,",j:",j))
     #print(paste("nav_lat",zoom_nav_lat[i,j]))
    }
}
print(paste("Start to mapping the data from 2D-matrix to 1D land compress"))
#======================================================================================

#=====TEMP==========
# pdf("test.pdf")
# plot(1,1, type="n", xlim=c(1,130) ,ylim=c(1,130))
#===================

#======= start to maping the values =================================================== 
ll<-1
tt<-1
for (tt in 1:ntstep) {
    for (  ll in 1:nland ) {    
	xid <- (land[ll] %%  nx)
        yid <- floor(land[ll]/nx)+1    # get the integer for the line
        # dealing with the xid=zero condition shift to previous line (yid)     
            if (xid == 0) {
                xid <- nx        #shift  the  x index to the lastest one
                yid <- yid - 1   #assign the  y index to previous line 
            }  
        #if (tt ==1 ){
        #print( paste("x:",xid, "y:",yid))
        #bb[ll]<-v1[xid,yid,1] #dummy variable use to check mapping variable aa
  	zoom_swdown   [ll,tt] <- v3[xid, yid, tt] 
	zoom_lwdown   [ll,tt] <- v2[xid, yid, tt] 
	zoom_rainfall [ll,tt] <- v7[xid, yid, tt] 
	zoom_snowfall [ll,tt] <- v6[xid, yid, tt] 
	zoom_qair     [ll,tt] <- v9[xid, yid, tt] 
	zoom_psurf    [ll,tt] <- v8[xid, yid, tt] 
	zoom_tair     [ll,tt] <- v1[xid, yid, tt] 
	zoom_wind_e   [ll,tt] <- v4[xid, yid, tt] 
	zoom_wind_n   [ll,tt] <- v5[xid, yid, tt] 
        #} #if tt ==1  
        #print(paste("x:",xid,ll"y:",yid))
       # points(xid,yid, cex=0.5) # plot the land points on the pdf test.pdf 
    } # land mask sequency
} # time loop 
#======dev.off()

	print("Finished compressing the 2D spatial dataset to 1D compressed format.")

#====== setup a NCDF file "define dimensions and  variables" 
	#====== define the  dimension =============================================
	dimX <- dim.def.ncdf("x","",  1:nx, create_dimvar=FALSE    )
	dimY <- dim.def.ncdf("y","",  1:ny, create_dimvar=FALSE    )
	dimT <- dim.def.ncdf("tstep", "seconds",   1:ntstep,unlim=TRUE)
	mask <- dim.def.ncdf("land", "",            1:nland )
	mv <- 1e+20
	set1 <- var.def.ncdf( "nav_lon", "degrees_east",list(dimX,dimY),longname="Longitude"  , mv  )
	set2 <- var.def.ncdf( "nav_lat", "degrees_north",list(dimX,dimY),longname="Latitude" , mv  )
	set3 <- var.def.ncdf( "time",    
			     paste("seconds since ",time_origin,sep="") ,dimT,longname="Time_axis", mv)
	set4 <- var.def.ncdf( "timestp", 
			     paste("timestep since ",time_origin,sep=""),dimT,longname="Time step", mv)
	set5 <- var.def.ncdf("lsmask","lsmask",list(dimX, dimY), longname="land_sea mask", mv )
	mv <- 1e+20
	vv1 <- var.def.ncdf( "SWdown", "W/m^2/s", list(mask, dimT),longname="SWdown", mv)
	vv2 <- var.def.ncdf( "LWdown", "W/m^2/s", list(mask, dimT),longname="LWdown", mv)
	vv3 <- var.def.ncdf( "Rainf", "kg/m^2/s", list(mask, dimT),longname="Rainf" , mv)
	vv4 <- var.def.ncdf( "Snowf", "kg/m^2/s", list(mask, dimT),longname="Snowf" , mv)
	vv5 <- var.def.ncdf( "Qair", "kg/kg",     list(mask, dimT),longname="Qair" ,  mv)
	vv6 <- var.def.ncdf( "PSurf", "Pa",       list(mask, dimT),longname="PSurf" , mv)
	vv7 <- var.def.ncdf( "Tair", "K",         list(mask, dimT),longname="Tair" ,  mv)
	vv8 <- var.def.ncdf( "Wind_E", "m/s",     list(mask, dimT),longname="Wind_E" ,mv)
	vv9 <- var.def.ncdf( "Wind_N", "m/s",     list(mask, dimT),longname="Wind_N" ,mv)


	#==== creat the nc file based on definition ========================== 
	names<-list(set1,set2,set3,set4, set5, vv1, vv2,vv3,vv4,vv5,vv6,vv7,vv8,vv9)
	#setting the input_information from "out_filename"  
	output_nc <- create.ncdf( out_filename, names)
	#=====================================================================

	#===== set attributes of variables in nc files for various types =====
	#longitude
	#att.put.ncdf( output_nc, set1, "title","lon" ,               prec="text"   ) 
	#att.put.ncdf( output_nc, set1, "axis", "X"       ,           prec="text"   )
	#att.put.ncdf( output_nc, set1, "valid_max", 180.0  ,         prec="single" )
	#att.put.ncdf( output_nc, set1, "valid_min",-180.0  ,         prec="single" )
	#att.put.ncdf( output_nc, set1, "modolo", 360.0,              prec="single" )
	#att.put.ncdf( output_nc, set1, "topology","circular",        prec="text"   )
	#att.put.ncdf( output_nc, set1, "_CoordinateAxisType", "Lon", prec="text"   )
	#latitude
	#att.put.ncdf( output_nc, set2, "title","lat"  , prec="text"   )
	#att.put.ncdf( output_nc, set2, "axis", "Y"       , prec="text"   ) 
	#att.put.ncdf( output_nc, set2, "valid_max", 90.0   , prec="single" )
	#att.put.ncdf( output_nc, set2, "valid_min",-90.0   , prec="single" )
	#att.put.ncdf( output_nc, set2, "_CoordinateAxisType", "Lat", prec="text") 

	#longitude
	att.put.ncdf( output_nc, set1, "title","Longitude" , prec="text"   )   
	att.put.ncdf( output_nc, set1, "axis", "TYX"       , prec="text"   )   
	att.put.ncdf( output_nc, set1, "valid_max", 180.0  , prec="single" )
	att.put.ncdf( output_nc, set1, "valid_min",-180.0  , prec="single" )
	#latitude
	att.put.ncdf( output_nc, set2, "title","Latitude"  , prec="text"   )   
	att.put.ncdf( output_nc, set2, "axis", "TYX"       , prec="text"   )   
	att.put.ncdf( output_nc, set2, "valid_max", 90.0   , prec="single" )
	att.put.ncdf( output_nc, set2, "valid_min",-90.0   , prec="single" )
	#time
	att.put.ncdf( output_nc, set3, "title",   "Time"   , prec="text"   ) 
	att.put.ncdf( output_nc, set3, "calendar","noleap" , prec="text"   )
	att.put.ncdf( output_nc, set3, "time_origin", time_origin   , prec="text" )
	#time step
	att.put.ncdf( output_nc, set4, "title", "Time"     , prec="text"   )
	att.put.ncdf( output_nc, set4, "tstep_sec", 21600.0   , prec="single" )
	att.put.ncdf( output_nc, set4, "time_origin", time_origin   , prec="text" )

	#land mask
	#att.put.ncdf( output_nc, set5, "axis", "YX", prec="text") 
	att.put.ncdf( output_nc, mask, "compress",'y x', prec="text"   )
	#short wave radiation
	att.put.ncdf( output_nc, vv1, "title", "Incoming_Short_Wave_Radiation" , prec="text" )
	att.put.ncdf( output_nc, vv1, "axis", "TYX"    , prec="text"   )
	#long wave radiation
	att.put.ncdf( output_nc, vv2, "title", "Incoming_Long_Wave_Radiation" , prec="text" )
	att.put.ncdf( output_nc, vv2, "axis", "TYX"    , prec="text" )
	#rainfall
	att.put.ncdf( output_nc, vv3, "title", "Rainfall rate" , prec="text" )
	att.put.ncdf( output_nc, vv3, "axis", "TYX"    , prec="text" )
	#snowfall
	att.put.ncdf( output_nc, vv4, "title", "Snowfall rate" , prec="text" )
	att.put.ncdf( output_nc, vv4, "axis", "TYX"    , prec="text" )
	#specific humidity
	att.put.ncdf( output_nc, vv5, "title", "Air_Specific_Humidity" , prec="text" )
	att.put.ncdf( output_nc, vv5, "axis", "TYX"    , prec="text" )
	#air pressure
	att.put.ncdf( output_nc, vv6, "title", "Pressure" , prec="text" )
	att.put.ncdf( output_nc, vv6, "axis", "TYX"    , prec="text" )
	#air temperature
	att.put.ncdf( output_nc, vv7, "title", "Temperature" , prec="text" )
	att.put.ncdf( output_nc, vv7, "axis", "TYX"    , prec="text" )
	#wind 
	att.put.ncdf( output_nc, vv8, "title", "U_wind_component" , prec="text" )
	att.put.ncdf( output_nc, vv8, "axis", "TYX" , prec="text" )
	att.put.ncdf( output_nc, vv9, "title", "V_wind_component" , prec="text" )
	att.put.ncdf( output_nc, vv9, "axis", "TYX" , prec="text" )
	#att.put.ncdf( output_nc, vv9, "associate", "time (nav_" , prec="text" )

	print(paste("Writing the dataset to nc file."))
	#put coordinate & time  
	put.var.ncdf(output_nc, set1, zoom_nav_lon      )
	put.var.ncdf(output_nc, set2, zoom_nav_lat      )
	put.var.ncdf(output_nc, set3, time_start_second )
	put.var.ncdf(output_nc, set4, time_start_steps  )
	#put land sea mask from ascii file 
	put.var.ncdf(output_nc, set5, mask_id_arr ) 
	#put compressed land index 
	put.var.ncdf(output_nc, mask, d4)
	#put forcing data
	put.var.ncdf(output_nc, vv1, zoom_swdown   )
	put.var.ncdf(output_nc, vv2, zoom_lwdown   )
	put.var.ncdf(output_nc, vv3, zoom_rainfall )
	put.var.ncdf(output_nc, vv4, zoom_snowfall )			
	put.var.ncdf(output_nc, vv5, zoom_qair     )
	put.var.ncdf(output_nc, vv6, zoom_psurf    )
	put.var.ncdf(output_nc, vv7, zoom_tair     )
	put.var.ncdf(output_nc, vv8, zoom_wind_e   )
	put.var.ncdf(output_nc, vv9, zoom_wind_n   )
	#close the file
	close.ncdf(output_nc)
	#print(paste("Finished zoom grid io transfering for ",in_filename))
	#print(paste("Produced ouput file:", out_filename) )
	#} # skip mark end

	#finished
	print(paste("create ", out_filename, "!", sep="") )
	#rm(list=ls()) 


	print(paste("This script finined the job on: ", Sys.time(),sep="")) 

}
}


#} 
#========== End of function fun_compress ======================


#========== Set the script====================================== 
# Start convert the file by fun_compress with specific argunment
# If you want to submit the file by queue to obelix, you need to apply the 
# following lines, which allowed this R-script can be called under the shell script
# with the arggunmets sending by specific batch jobs  
#arg<-commandArgs(TRUE)
#fun_compress(arg[1], arg[2]) 
#========== End ================================================





