#!/usr/bin/env Rscript
#  capacity.R script calling functions to generate and model/simulate the scenario
# Copyright (C) 2017-2018  Mays AL-Naday
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' Parse Input options ------------------------------------------
#' parse input arguments.
option_list = list(
  optparse::make_option(c("-c", "--config"), type="character", default=NA,
              help="Model configuration file", metavar="character"),
  optparse::make_option(c("-e", "--enviroment"), type="character", default="default",
              help="the default enviorment (or section) of the config file [default= %default]", metavar="character"),
  optparse::make_option(c("-l", "--log"), type="character", default = "INFO",
              help="Default logging level [default=%default]", metavar = "character")
);
opt_parser = optparse::OptionParser(option_list=option_list);
opt = optparse::parse_args(opt_parser);
if (is.na(opt$config)){
  optparse::print_help(opt_parser)
  stop("The config file must be supplied <CONFIG_NAME>.yml", call.=FALSE)
}
logging::basicConfig(opt$log)
logging::logdebug("config file is: %s", opt$config)
#' ------------------------------------------
#' Parse the config file of the simulation
#' create an arguments enviroment where different input arguements are bind
gcfgs <- ice::ParseConfig(config.file = opt$config, config.active = opt$log, config.env = 'default')
scfgs <- ice::ParseConfig(config.file = opt$config, config.active = opt$log, config.env = opt$enviroment)
#' verify input data is already generated and stored, if not then generate and save the data.
ice::GenSimData(cfgs = gcfgs)
#' Simulate capacity requrirments for admitting input unicast traffic in ICN
if('icn.uc' %in% scfgs$tcfg$v)
  ice::SimIcnUnicast(scfgs)
#' Simulate capacity requrirments for admitting input multicast traffic in ICN
if('icn.mc' %in% scfgs$tcfg$v)
  ice::SimIcnMulticast(scfgs)
#' Simulate capacity requirments for admitting input traffic in IP
if('ip' %in% scfgs$tcfg$v)
  ice::SimIp(scfgs)
loginfo('SCRIPT: Simulation complete for config: %s', opt$config)
