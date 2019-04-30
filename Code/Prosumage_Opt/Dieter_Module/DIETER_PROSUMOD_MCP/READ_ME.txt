#####################################################################################
################## Prosumage tariff design scenarios ################################
##                                                                                 ##
## This folder contains a MCP version of DIETER with a detailed prosumage segment  ##
##                                                                                 ##
##                                                                                 ##
## Techncial requirements: GAMS license, CPLEX and PATH solver licence             ##
#####################################################################################

A) Selection of scenarios:
To select a tariff scenario, change parameters in the 'tariff_scenario.gms' file and save file.


B) Explanation of solution strategy:
To speed-up solution of the MCP model, a step-wise solution approach is implemented:

- File 'DIETER_prerun.gms' slices the set of hours and solves them individually before jointly solving the entire hour set. 
Additionally, 'DIETER_prerun.gms'relies on the model version 'model_prerun.gms', which is only based on inequalities (i.e. strictly positive dual variables). 
This speeds up finding preliminary solutions, whose points are saved in 'DIETER_MCP_p'.

- File 'DIETER.gms' uses 'DIETER_MCP_p' as starting points. 
'DIETER.gms' sources the 'model.gms' file, which contains the final model with inequality and equality constraints (i.e. free and positive duals).
'DIETER.gms' creates the final results file.

C) Running scenarios
There are two options for running scenarios:
- 1) Manual implementation: First run 'DIETER_prerun.gms'. This should produce the preliminary solution points 'DIETER_MCP_p'. 
Then run 'DIETER.gms'. This should produce the final results file 'results.gdx'.
- 2) Automatic implementation with batch script: Check with text editor of your choice whether the path in 'RUN.bat'corresponds to the path for GAMS installation on your computer. 
If not, add the correct path and save. Run batch script (double click). 
This should first produce the preliminary solution points 'DIETER_MCP_p' and later on the final results file 'results.gdx'.


