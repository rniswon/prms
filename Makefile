# prms library makefile
# $Id: Makefile 7690 2015-10-26 19:02:57Z rsregan $

include ../makelist
TARGET 	= .$(BINDIR)/prmsV
TRUNK   = .$(TRUNKDIR)

####################################################
# Rules for targets
####################################################
all: $(TARGET)

#
# Define all object files which make up the library
#

OBJS = \
        call_modules.o \
        basin.o \
        climateflow.o \
        cascade.o \
        soltab.o \
        setup_param.o \
        prms_time.o \
        obs.o \
        climate_hru.o \
        potet_jh.o \
        potet_pt.o \
        potet_hs.o \
        potet_pm.o \
        potet_pm_sta.o \
        potet_pan.o \
        potet_hamon.o \
        ddsolrad.o \
        ccsolrad.o \
        ide_dist.o \
        xyz_dist.o \
        precip_1sta_laps.o \
        precip_temp_grid.o \
        precip_dist2.o \
        temp_1sta_laps.o \
        temp_dist2.o \
        transp_frost.o \
        transp_tindex.o \
        frost_date.o \
        intcp.o \
        snowcomp.o \
        srunoff.o \
        soilzone.o \
        gwflow.o \
        water_use_read.o \
        dynamic_param_read.o \
        water_balance.o \
        routing.o \
        strmflow.o \
        strmflow_in_out.o \
        muskingum.o \
        muskingum_lake.o \
        subbasin.o \
        map_results.o \
        nhru_summary.o \
        write_climate_hru.o \
        prms_summary.o \
        basin_sum.o \
        utils_prms.o

$(TARGET): $(OBJS)
	$(RM) $(TARGET)
	$(FC) $(LDFLAGS) -o $(TARGET) $(OBJS) $(MMFLIB) $(FLIBS)

clean:
	$(RM) $(TARGET) *.o *.mod *~

call_modules.o: call_modules.f90
	$(FC) -c $(FFLAGS) call_modules.f90

setup_param.o: setup_param.f90 prms_module.mod prms_basin.mod
	$(FC) -c $(FFLAGS) setup_param.f90

snowcomp.o: snowcomp.f90 prms_module.mod prms_basin.mod prms_flowvars.mod prms_climatevars.mod prms_set_time.mod prms_intcp.mod
	$(FC) -c $(FFLAGS) snowcomp.f90

basin_sum.o: basin_sum.f90 prms_module.mod prms_basin.mod prms_flowvars.mod prms_intcp.mod prms_snow.mod prms_srunoff.mod prms_gwflow.mod prms_climatevars.mod prms_set_time.mod prms_obs.mod prms_muskingum.mod
	$(FC) -c $(FFLAGS) basin_sum.f90

ddsolrad.o: ddsolrad.f90 prms_module.mod prms_climatevars.mod prms_soltab.mod prms_set_time.mod prms_obs.mod
	$(FC) -c $(FFLAGS) ddsolrad.f90

ccsolrad.o: ccsolrad.f90 prms_module.mod prms_basin.mod prms_climatevars.mod prms_soltab.mod prms_set_time.mod prms_obs.mod
	$(FC) -c $(FFLAGS) ccsolrad.f90

gwflow.o: gwflow.f90 prms_module.mod prms_basin.mod prms_flowvars.mod prms_intcp.mod prms_srunoff.mod prms_soilzone.mod prms_cascade.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) gwflow.f90

prms_summary.o: prms_summary.f90 prms_module.mod prms_climatevars.mod prms_flowvars.mod prms_set_time.mod prms_obs.mod prms_intcp.mod prms_snow.mod prms_srunoff.mod prms_soilzone.mod prms_gwflow.mod
	$(FC) -c $(FFLAGS) prms_summary.f90

muskingum.o: muskingum.f90 routing.o prms_module.mod prms_basin.mod prms_flowvars.mod prms_set_time.mod prms_obs.mod prms_routing.mod prms_srunoff.mod prms_gwflow.mod
	$(FC) -c $(FFLAGS) muskingum.f90

intcp.o: intcp.f90 prms_obs.mod prms_climatevars.mod prms_flowvars.mod prms_module.mod prms_basin.mod prms_water_use.mod prms_set_time.mod prms_obs.mod
	$(FC) -c $(FFLAGS) intcp.f90

nhru_summary.o: nhru_summary.f90 prms_module.mod prms_basin.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) nhru_summary.f90

soltab.o: soltab.f90 prms_module.mod prms_basin.mod
	$(FC) -c $(FFLAGS) soltab.f90

climate_hru.o: climate_hru.f90 prms_module.mod prms_basin.mod prms_soltab.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) climate_hru.f90

cascade.o: cascade.f90 prms_module.mod prms_basin.mod
	$(FC) -c $(FFLAGS) cascade.f90

basin.o: basin.f90 prms_module.mod
	$(FC) -c $(FFLAGS) basin.f90

obs.o: obs.f90 prms_module.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) obs.f90

srunoff.o: srunoff.f90 prms_module.mod prms_basin.mod prms_flowvars.mod prms_climatevars.mod prms_intcp.mod prms_snow.mod prms_cascade.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) srunoff.f90

climateflow.o: climateflow.f90 prms_module.mod prms_basin.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) climateflow.f90

soilzone.o: soilzone.f90 prms_module.mod prms_basin.mod
	$(FC) -c $(FFLAGS) soilzone.f90

routing.o: routing.f90 prms_module.mod prms_basin.mod prms_gwflow.mod prms_flowvars.mod
	$(FC) -c $(FFLAGS) routing.f90

map_results.o: map_results.f90 prms_module.mod prms_basin.mod prms_set_time.mod
	$(FC) -c $(FFLAGS) map_results.f90

ide_dist.o: ide_dist.f prms_module.mod prms_basin.mod prms_set_time.mod prms_climatevars.mod prms_obs.mod
	$(FC) -c $(FFLAGS) ide_dist.f

precip_1sta_laps.o: precip_1sta_laps.f90 prms_module.mod prms_basin.mod prms_set_time.mod prms_climatevars.mod prms_obs.mod
	$(FC) -c $(FFLAGS) precip_1sta_laps.f90

prms_time.o: $(TRUNK)/prms_set_time.mod

muskingum_lake.o: muskingum_lake.f90 prms_module.mod prms_basin.mod prms_flowvars.mod prms_set_time.mod prms_routing.mod prms_climatevars.mod prms_obs.mod prms_srunoff.mod prms_soilzone.mod prms_gwflow.mod 
	$(FC) -c $(FFLAGS) muskingum_lake.f90

water_balance.o: water_balance.f90 prms_module.mod prms_basin.mod prms_srunoff.mod prms_flowvars.mod prms_gwflow.mod prms_climatevars.mod prms_set_time.mod prms_cascade.mod prms_intcp.mod prms_snow.mod prms_soilzone.mod
	$(FC) -c $(FFLAGS) water_balance.f90

dynamic_param_read.o: dynamic_param_read.f90 prms_module.mod prms_basin.mod prms_set_time.mod prms_climatevars.mod prms_flowvars.mod prms_potet_jh.mod prms_potet_pm.mod prms_potet_hs.mod prms_potet_pt.mod prms_potet_hamon.mod transp_tindex.o transp_frost.o prms_intcp.mod prms_snow.mod prms_srunoff.mod prms_soilzone.mod
	$(FC) -c $(FFLAGS) dynamic_param_read.f90

water_use_read.o: water_use_read.f90 prms_module.mod prms_basin.mod prms_set_time.mod prms_flowvars.mod
	$(FC) -c $(FFLAGS) water_use_read.f90

prms_climatevars.mod: climateflow.o
prms_flowvars.mod: climateflow.o
prms_module.mod: call_modules.o
prms_gwflow.mod: gwflow.o
prms_basin.mod: basin.o
prms_soltab.mod: soltab.o
prms_intcp.mod: intcp.o
prms_cascade.mod: cascade.o
prms_srunoff.mod: srunoff.o
prms_soilzone.mod: soilzone.o
prms_snowcomp.mod: snowcomp.o
prms_routing.mod: routing.o
prms_water_use.mod: water_use_read.o
prms_obs.mod: obs.o
utils_prms.o: $(TRUNK)/utils_prms.o
	$(CP) $(TRUNK)/utils_prms.o .
xyz_dist.o: $(TRUNK)/xyz_dist.o
	$(CP) $(TRUNK)/xyz_dist.o .
temp_1sta_laps.o: $(TRUNK)/temp_1sta_laps.o
	$(CP) $(TRUNK)/temp_1sta_laps.o .
subbasin.o: $(TRUNK)/subbasin.o
	$(CP) $(TRUNK)/subbasin.o .
transp_tindex.o: $(TRUNK)/transp_tindex.o
	$(CP) $(TRUNK)/prms_transp_tindex.mod $(TRUNK)/transp_tindex.o .
transp_frost.o: $(TRUNK)/transp_frost.o
	$(CP) $(TRUNK)/prms_transp_frost.mod $(TRUNK)/transp_frost.o .
frost_date.o: $(TRUNK)/frost_date.o
	$(CP) $(TRUNK)/frost_date.o .
write_climate_hru.o: $(TRUNK)/write_climate_hru.o
	$(CP) $(TRUNK)/write_climate_hru.o .
temp_dist2.o: $(TRUNK)/temp_dist2.o
	$(CP) $(TRUNK)/temp_dist2.o .
precip_dist2.o: $(TRUNK)/precip_dist2.o
	$(CP) $(TRUNK)/precip_dist2.o .
strmflow.o: $(TRUNK)/strmflow.o
	$(CP) $(TRUNK)/strmflow.o .
strmflow_in_out.o: $(TRUNK)/strmflow_in_out.o
	$(CP) $(TRUNK)/strmflow_in_out.o .
potet_jh.o: $(TRUNK)/potet_jh.o
	$(CP) $(TRUNK)/prms_potet_jh.mod $(TRUNK)/potet_jh.o .
potet_pt.o: $(TRUNK)/potet_pt.o
	$(CP) $(TRUNK)/prms_potet_pt.mod $(TRUNK)/potet_pt.o .
potet_hs.o: $(TRUNK)/potet_hs.o
	$(CP) $(TRUNK)/prms_potet_hs.mod $(TRUNK)/potet_hs.o .
potet_pm.o: $(TRUNK)/potet_pm.o
	$(CP) $(TRUNK)/prms_potet_pm.mod $(TRUNK)/potet_pm.o .
potet_hamon.o: $(TRUNK)/potet_hamon.o
	$(CP) $(TRUNK)/prms_potet_hamon.mod $(TRUNK)/potet_hamon.o .
potet_pan.o: $(TRUNK)/potet_pan.o
	$(CP) $(TRUNK)/prms_potet_pan.mod $(TRUNK)/potet_pan.o .
prms_set_time.mod: $(TRUNK)/prms_time.o
	$(CP) $(TRUNK)/prms_set_time.mod $(TRUNK)/prms_time.o .

