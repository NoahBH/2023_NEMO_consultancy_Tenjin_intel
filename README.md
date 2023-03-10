# 2023_NEMO_consultancy_Tenjin_intel
This is the repository meant for our clients.



# **SITUATION**  
Science versus Corona has means to create pedestrian models to simulate individual movement and crowd behavior by:                                         

-   Assembling a mobile real-time locating system (RTLS) that allows gathering movement data (XY-coordinates) in real-time                                 
-   Acquiring at a frequency of up to 8 Hz and used as input for the M4MA model                                                                              
-   Gathering 8 Hz data in a range of settings where agents move around to complete different goals                                                         


# **COMPLICATION** 
-   They have optimized the system by using KINEXON system, which they are not sure of the quality of the data                                             
-   The recorded data had missing data points -- indicating an issue with the recording setup                                                                
-   Missing data may contain (an)isotropic noise                                                                                                            
-   Potential issues for causing the noise or systematic errors are unknown                                                                                 

# **REQUEST** 
1.  The clients want to know the quality of the data and the potential impact of systematic distortions, signal drop-out, and measurement noise.           
2.  They would like an analysis pipeline to investigate these potential quality issues and to provide a smoothed and interpolated location time series, which would be used to fit a model of \~2Hz step decisions. 
