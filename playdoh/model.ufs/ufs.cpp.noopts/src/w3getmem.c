   
   
   
   
   
   
   
                                        
   
   
   
   
   
   
   
   
   
	#define __deprecated_msg(_msg) __attribute__((__deprecated__(_msg)))
	#define __deprecated_enum_msg(_msg) __deprecated_msg(_msg)
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
                                          
   
   
   
   
   
   
   
   
   
   
   
   
   
 
 
   
 
 
 #define __api_to_be_deprecated 100000
 
 
   
 
   
   
  
    #if defined(0 && 1
        
            #define __mac_os_x_version_min_required 140000
            
        #endif
    
    #if defined(0 && 1
        
    #elif  __environment_iphone_os_version_min_required__ 
        
        #define __iphone_os_version_max_allowed 170500
    
    #if defined(0 && 1
        
    #elif  __environment_watch_os_version_min_required__ 
        
        #define __watch_os_version_max_allowed 100500
        
        
    #endif 
    #if defined(0 && 1
        
    #elif  __environment_tv_os_version_min_required__ 
        
        #define __tv_os_version_max_allowed 170500
        
        
    #endif 
    #if defined(0 && 1
        
    #endif 
    #if defined(0 && 1
        
    #endif 
    #if defined(0 && 1
        
    #endif 
//fixme: workaround for rdar://116062344
    #if defined(0 && 1
        
    #endif 
    
    
        #define __iphone_os_version_max_allowed     170000
    
    
    
    #if 1
        
    #else
        
    #endif
   
  
 #if 1
  
   #if 1
    
     #if 1
      
       #if (1 && 1 && ((0 && 0) || (0 && 0)))
         
         #define __enable_legacy_mac_availability 1
       
      #endif 
     
    #endif 
   
  #endif 
 
  #define __enable_legacy_mac_availability 1
 
    #if defined(0 && defined(0
        
            
            
            #define __availability_internal__iphone_2_0_dep__iphone_10_0   __attribute__((availability(ios,introduced=2.0,deprecated=10.0)))
            
                #define __availability_internal__iphone_2_0_dep__iphone_10_0_msg(_msg)   __attribute__((availability(ios,introduced=2.0,deprecated=10.0,message=_msg)))
            
            #define __availability_internal__iphone_2_0_dep__iphone_10_1   __attribute__((availability(ios,introduced=2.0,deprecated=10.1)))
            
                #define __availability_internal__iphone_2_0_dep__iphone_10_1_msg(_msg)   __attribute__((availability(ios,introduced=2.0,deprecated=10.1,message=_msg)))
            
            #define __availability_internal__iphone_2_0_dep__iphone_10_2   __attribute__((availability(ios,introduced=2.0,deprecated=10.2)))
            
                #define __availability_internal__iphone_2_0_dep__iphone_10_2_msg(_msg)   __attribute__((availability(ios,introduced=2.0,deprecated=10.2,message=_msg)))
            
            #define __availability_internal__iphone_2_0_dep__iphone_10_3   __attribute__((availability(ios,introduced=2.0,deprecated=10.3)))
            
                #define __availability_internal__iphone_2_0_dep__iphone_10_3_msg(_msg)   __attribute__((availability(ios,introduced=2.0,deprecated=10.3,message=_msg)))
            
            #define __availability_internal__iphone_2_0_dep__iphone_11_0   __attribute__((availability(ios,introduced=2.0,deprecated=11.0)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__iphone_2_0_dep__iphone_na_msg(_msg)   __attribute__((availability(ios,introduced=2.0)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_10_0   __attribute__((availability(ios,introduced=2.1,deprecated=10.0)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_10_0_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=10.0,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_10_1   __attribute__((availability(ios,introduced=2.1,deprecated=10.1)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_10_1_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=10.1,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_10_2   __attribute__((availability(ios,introduced=2.1,deprecated=10.2)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_10_2_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=10.2,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_10_3   __attribute__((availability(ios,introduced=2.1,deprecated=10.3)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_10_3_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=10.3,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_2_1   __attribute__((availability(ios,introduced=2.1,deprecated=2.1)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_2_1_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=2.1,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_2_2   __attribute__((availability(ios,introduced=2.1,deprecated=2.2)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_2_2_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=2.2,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_3_0   __attribute__((availability(ios,introduced=2.1,deprecated=3.0)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_3_0_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=3.0,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_3_1   __attribute__((availability(ios,introduced=2.1,deprecated=3.1)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_3_1_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=3.1,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_3_2   __attribute__((availability(ios,introduced=2.1,deprecated=3.2)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_3_2_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=3.2,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_4_0   __attribute__((availability(ios,introduced=2.1,deprecated=4.0)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_4_0_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=4.0,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_4_1   __attribute__((availability(ios,introduced=2.1,deprecated=4.1)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_4_1_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=4.1,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_4_2   __attribute__((availability(ios,introduced=2.1,deprecated=4.2)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_4_2_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=4.2,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_4_3   __attribute__((availability(ios,introduced=2.1,deprecated=4.3)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_4_3_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=4.3,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_5_0   __attribute__((availability(ios,introduced=2.1,deprecated=5.0)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_5_0_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=5.0,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_5_1   __attribute__((availability(ios,introduced=2.1,deprecated=5.1)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_5_1_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=5.1,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_6_0   __attribute__((availability(ios,introduced=2.1,deprecated=6.0)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_6_0_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=6.0,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_6_1   __attribute__((availability(ios,introduced=2.1,deprecated=6.1)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_6_1_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=6.1,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_7_0   __attribute__((availability(ios,introduced=2.1,deprecated=7.0)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_7_0_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=7.0,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_7_1   __attribute__((availability(ios,introduced=2.1,deprecated=7.1)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_7_1_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=7.1,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_8_0   __attribute__((availability(ios,introduced=2.1,deprecated=8.0)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_8_0_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=8.0,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_8_1   __attribute__((availability(ios,introduced=2.1,deprecated=8.1)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_8_1_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=8.1,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_8_2   __attribute__((availability(ios,introduced=2.1,deprecated=8.2)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_8_2_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=8.2,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_8_3   __attribute__((availability(ios,introduced=2.1,deprecated=8.3)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_8_3_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=8.3,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_8_4   __attribute__((availability(ios,introduced=2.1,deprecated=8.4)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_8_4_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=8.4,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_9_0   __attribute__((availability(ios,introduced=2.1,deprecated=9.0)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_9_0_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=9.0,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_9_1   __attribute__((availability(ios,introduced=2.1,deprecated=9.1)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_9_1_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=9.1,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_9_2   __attribute__((availability(ios,introduced=2.1,deprecated=9.2)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_9_2_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=9.2,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_9_3   __attribute__((availability(ios,introduced=2.1,deprecated=9.3)))
            
                #define __availability_internal__iphone_2_1_dep__iphone_9_3_msg(_msg)   __attribute__((availability(ios,introduced=2.1,deprecated=9.3,message=_msg)))
            
            #define __availability_internal__iphone_2_1_dep__iphone_na   __attribute__((availability(ios,introduced=2.1)))
            
            #define __availability_internal__iphone_2_2                    __attribute__((availability(ios,introduced=2.2)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__iphone_2_2_dep__iphone_na_msg(_msg)   __attribute__((availability(ios,introduced=2.2)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_10_0   __attribute__((availability(ios,introduced=3.0,deprecated=10.0)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_10_0_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=10.0,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_10_1   __attribute__((availability(ios,introduced=3.0,deprecated=10.1)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_10_1_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=10.1,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_10_2   __attribute__((availability(ios,introduced=3.0,deprecated=10.2)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_10_2_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=10.2,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_10_3   __attribute__((availability(ios,introduced=3.0,deprecated=10.3)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_10_3_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=10.3,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_3_0   __attribute__((availability(ios,introduced=3.0,deprecated=3.0)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_3_0_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=3.0,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_3_1   __attribute__((availability(ios,introduced=3.0,deprecated=3.1)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_3_1_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=3.1,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_3_2   __attribute__((availability(ios,introduced=3.0,deprecated=3.2)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_3_2_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=3.2,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_4_0   __attribute__((availability(ios,introduced=3.0,deprecated=4.0)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_4_0_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=4.0,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_4_1   __attribute__((availability(ios,introduced=3.0,deprecated=4.1)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_4_1_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=4.1,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_4_2   __attribute__((availability(ios,introduced=3.0,deprecated=4.2)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_4_2_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=4.2,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_4_3   __attribute__((availability(ios,introduced=3.0,deprecated=4.3)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_4_3_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=4.3,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_5_0   __attribute__((availability(ios,introduced=3.0,deprecated=5.0)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_5_0_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=5.0,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_5_1   __attribute__((availability(ios,introduced=3.0,deprecated=5.1)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_5_1_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=5.1,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_6_0   __attribute__((availability(ios,introduced=3.0,deprecated=6.0)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_6_0_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=6.0,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_6_1   __attribute__((availability(ios,introduced=3.0,deprecated=6.1)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_6_1_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=6.1,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_7_0   __attribute__((availability(ios,introduced=3.0,deprecated=7.0)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_7_0_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=7.0,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_7_1   __attribute__((availability(ios,introduced=3.0,deprecated=7.1)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_7_1_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=7.1,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_8_0   __attribute__((availability(ios,introduced=3.0,deprecated=8.0)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_8_0_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=8.0,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_8_1   __attribute__((availability(ios,introduced=3.0,deprecated=8.1)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_8_1_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=8.1,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_8_2   __attribute__((availability(ios,introduced=3.0,deprecated=8.2)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_8_2_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=8.2,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_8_3   __attribute__((availability(ios,introduced=3.0,deprecated=8.3)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_8_3_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=8.3,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_8_4   __attribute__((availability(ios,introduced=3.0,deprecated=8.4)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_8_4_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=8.4,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_9_0   __attribute__((availability(ios,introduced=3.0,deprecated=9.0)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_9_0_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=9.0,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_9_1   __attribute__((availability(ios,introduced=3.0,deprecated=9.1)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_9_1_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=9.1,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_9_2   __attribute__((availability(ios,introduced=3.0,deprecated=9.2)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_9_2_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=9.2,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_9_3   __attribute__((availability(ios,introduced=3.0,deprecated=9.3)))
            
                #define __availability_internal__iphone_3_0_dep__iphone_9_3_msg(_msg)   __attribute__((availability(ios,introduced=3.0,deprecated=9.3,message=_msg)))
            
            #define __availability_internal__iphone_3_0_dep__iphone_na   __attribute__((availability(ios,introduced=3.0)))
            
            #define __availability_internal__iphone_3_1                    __attribute__((availability(ios,introduced=3.1)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__iphone_3_1_dep__iphone_na_msg(_msg)   __attribute__((availability(ios,introduced=3.1)))
            
            #define __availability_internal__iphone_3_2_dep__iphone_10_0   __attribute__((availability(ios,introduced=3.2,deprecated=10.0)))
            
                #define __availability_internal__iphone_3_2_dep__iphone_10_0_msg(_msg)   __attribute__((availability(ios,introduced=3.2,deprecated=10.0,message=_msg)))
            
            #define __availability_internal__iphone_3_2_dep__iphone_10_1   __attribute__((availability(ios,introduced=3.2,deprecated=10.1)))
            
                #define __availability_internal__iphone_3_2_dep__iphone_10_1_msg(_msg)   __attribute__((availability(ios,introduced=3.2,deprecated=10.1,message=_msg)))
            
            #define __availability_internal__iphone_3_2_dep__iphone_10_2   __attribute__((availability(ios,introduced=3.2,deprecated=10.2)))
            
                #define __availability_internal__iphone_3_2_dep__iphone_10_2_msg(_msg)   __attribute__((availability(ios,introduced=3.2,deprecated=10.2,message=_msg)))
            
            #define __availability_internal__iphone_3_2_dep__iphone_10_3   __attribute__((availability(ios,introduced=3.2,deprecated=10.3)))
            
                #define __availability_internal__iphone_3_2_dep__iphone_10_3_msg(_msg)   __attribute__((availability(ios,introduced=3.2,deprecated=10.3,message=_msg)))
            
            #define __availability_internal__iphone_3_2_dep__iphone_3_2   __attribute__((availability(ios,introduced=3.2,deprecated=3.2)))
            
                #define __availability_internal__iphone_3_2_dep__iphone_3_2_msg(_msg)   __attribute__((availability(ios,introduced=3.2,deprecated=3.2,message=_msg)))
            
            #define __availability_internal__iphone_3_2_dep__iphone_4_0   __attribute__((availability(ios,introduced=3.2,deprecated=4.0)))
            
                #define __availability_internal__iphone_3_2_dep__iphone_4_0_msg(_msg)   __attribute__((availability(ios,introduced=3.2,deprecated=4.0,message=_msg)))
            
            #define __availability_internal__iphone_3_2_dep__iphone_4_1   __attribute__((availability(ios,introduced=3.2,deprecated=4.1)))
            
                #define __availability_internal__iphone_3_2_dep__iphone_4_1_msg(_msg)   __attribute__((availability(ios,introduced=3.2,deprecated=4.1,message=_msg)))
            
            #define __availability_internal__iphone_3_2_dep__iphone_4_2   __attribute__((availability(ios,introduced=3.2,deprecated=4.2)))
            
                #define __availability_internal__iphone_3_2_dep__iphone_4_2_msg(_msg)   __attribute__((availability(ios,introduced=3.2,deprecated=4.2,message=_msg)))
            
            #define __availability_internal__iphone_3_2_dep__iphone_4_3   __attribute__((availability(ios,introduced=3.2,deprecated=4.3)))
            
                #define __availability_internal__iphone_3_2_dep__iphone_4_3_msg(_msg)   __attribute__((availability(ios,introduced=3.2,deprecated=4.3,message=_msg)))
            
            #define __availability_internal__iphone_3_2_dep__iphone_5_0   __attribute__((availability(ios,introduced=3.2,deprecated=5.0)))
            
                #define __availability_internal__iphone_3_2_dep__iphone_5_0_msg(_msg)   __attribute__((availability(ios,introduced=3.2,deprecated=5.0,message=_msg)))
            
            #define __availability_internal__iphone_3_2_dep__iphone_5_1   __attribute__((availability(ios,introduced=3.2,deprecated=5.1)))
            
                #define __availability_internal__iphone_3_2_dep__iphone_5_1_msg(_msg)   __attribute__((availability(ios,introduced=3.2,deprecated=5.1,message=_msg)))
            
            #define __availability_internal__iphone_3_2_dep__iphone_6_0   __attribute__((availability(ios,introduced=3.2,deprecated=6.0)))
            
                #define __availability_internal__iphone_3_2_dep__iphone_6_0_msg(_msg)   __attribute__((availability(ios,introduced=3.2,deprecated=6.0,message=_msg)))
            
            #define __availability_internal__iphone_3_2_dep__iphone_6_1   __attribute__((availability(ios,introduced=3.2,deprecated=6.1)))
            
                #define __availability_internal__iphone_3_2_dep__iphone_6_1_msg(_msg)   __attribute__((availability(ios,introduced=3.2,deprecated=6.1,message=_msg)))
            
            #define __availability_internal__iphone_3_2_dep__iphone_7_0   __attribute__((availability(ios,introduced=3.2,deprecated=7.0)))
            
                #define __availability_internal__iphone_3_2_dep__iphone_7_0_msg(_msg)   __attribute__((availability(ios,introduced=3.2,deprecated=7.0,message=_msg)))
            
            #define __availability_internal__iphone_3_2_dep__iphone_7_1   __attribute__((availability(ios,introduced=3.2,deprecated=7.1)))
            
                #define __availability_internal__iphone_3_2_dep__iphone_7_1_msg(_msg)   __attribute__((availability(ios,introduced=3.2,deprecated=7.1,message=_msg)))
            
            #define __availability_internal__iphone_3_2_dep__iphone_8_0   __attribute__((availability(ios,introduced=3.2,deprecated=8.0)))
            
                #define __availability_internal__iphone_3_2_dep__iphone_8_0_msg(_msg)   __attribute__((availability(ios,introduced=3.2,deprecated=8.0,message=_msg)))
            
            #define __availability_internal__iphone_3_2_dep__iphone_8_1   __attribute__((availability(ios,introduced=3.2,deprecated=8.1)))
            
                #define __availability_internal__iphone_3_2_dep__iphone_8_1_msg(_msg)   __attribute__((availability(ios,introduced=3.2,deprecated=8.1,message=_msg)))
            
            #define __availability_internal__iphone_3_2_dep__iphone_8_2   __attribute__((availability(ios,introduced=3.2,deprecated=8.2)))
            
                #define __availability_internal__iphone_3_2_dep__iphone_8_2_msg(_msg)   __attribute__((availability(ios,introduced=3.2,deprecated=8.2,message=_msg)))
            
            #define __availability_internal__iphone_3_2_dep__iphone_8_3   __attribute__((availability(ios,introduced=3.2,deprecated=8.3)))
            
                #define __availability_internal__iphone_3_2_dep__iphone_8_3_msg(_msg)   __attribute__((availability(ios,introduced=3.2,deprecated=8.3,message=_msg)))
            
            #define __availability_internal__iphone_3_2_dep__iphone_8_4   __attribute__((availability(ios,introduced=3.2,deprecated=8.4)))
            
                #define __availability_internal__iphone_3_2_dep__iphone_8_4_msg(_msg)   __attribute__((availability(ios,introduced=3.2,deprecated=8.4,message=_msg)))
            
            #define __availability_internal__iphone_3_2_dep__iphone_9_0   __attribute__((availability(ios,introduced=3.2,deprecated=9.0)))
            
                #define __availability_internal__iphone_3_2_dep__iphone_9_0_msg(_msg)   __attribute__((availability(ios,introduced=3.2,deprecated=9.0,message=_msg)))
            
            #define __availability_internal__iphone_3_2_dep__iphone_9_1   __attribute__((availability(ios,introduced=3.2,deprecated=9.1)))
            
                #define __availability_internal__iphone_3_2_dep__iphone_9_1_msg(_msg)   __attribute__((availability(ios,introduced=3.2,deprecated=9.1,message=_msg)))
            
            #define __availability_internal__iphone_3_2_dep__iphone_9_2   __attribute__((availability(ios,introduced=3.2,deprecated=9.2)))
            
                #define __availability_internal__iphone_3_2_dep__iphone_9_2_msg(_msg)   __attribute__((availability(ios,introduced=3.2,deprecated=9.2,message=_msg)))
            
            #define __availability_internal__iphone_3_2_dep__iphone_9_3   __attribute__((availability(ios,introduced=3.2,deprecated=9.3)))
            
                #define __availability_internal__iphone_3_2_dep__iphone_9_3_msg(_msg)   __attribute__((availability(ios,introduced=3.2,deprecated=9.3,message=_msg)))
            
            #define __availability_internal__iphone_3_2_dep__iphone_na   __attribute__((availability(ios,introduced=3.2)))
            
            #define __availability_internal__iphone_4_0                    __attribute__((availability(ios,introduced=4.0)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
                #define __availability_internal__iphone_4_0_dep__iphone_12_0_msg(_msg)   __attribute__((availability(ios,introduced=4.0,deprecated=12.0,message=_msg)))
            
            #define __availability_internal__iphone_4_0_dep__iphone_4_0   __attribute__((availability(ios,introduced=4.0,deprecated=4.0)))
            
                #define __availability_internal__iphone_4_0_dep__iphone_4_0_msg(_msg)   __attribute__((availability(ios,introduced=4.0,deprecated=4.0,message=_msg)))
            
            #define __availability_internal__iphone_4_0_dep__iphone_4_1   __attribute__((availability(ios,introduced=4.0,deprecated=4.1)))
            
                #define __availability_internal__iphone_4_0_dep__iphone_4_1_msg(_msg)   __attribute__((availability(ios,introduced=4.0,deprecated=4.1,message=_msg)))
            
            #define __availability_internal__iphone_4_0_dep__iphone_4_2   __attribute__((availability(ios,introduced=4.0,deprecated=4.2)))
            
                #define __availability_internal__iphone_4_0_dep__iphone_4_2_msg(_msg)   __attribute__((availability(ios,introduced=4.0,deprecated=4.2,message=_msg)))
            
            #define __availability_internal__iphone_4_0_dep__iphone_4_3   __attribute__((availability(ios,introduced=4.0,deprecated=4.3)))
            
                #define __availability_internal__iphone_4_0_dep__iphone_4_3_msg(_msg)   __attribute__((availability(ios,introduced=4.0,deprecated=4.3,message=_msg)))
            
            #define __availability_internal__iphone_4_0_dep__iphone_5_0   __attribute__((availability(ios,introduced=4.0,deprecated=5.0)))
            
                #define __availability_internal__iphone_4_0_dep__iphone_5_0_msg(_msg)   __attribute__((availability(ios,introduced=4.0,deprecated=5.0,message=_msg)))
            
            #define __availability_internal__iphone_4_0_dep__iphone_5_1   __attribute__((availability(ios,introduced=4.0,deprecated=5.1)))
            
                #define __availability_internal__iphone_4_0_dep__iphone_5_1_msg(_msg)   __attribute__((availability(ios,introduced=4.0,deprecated=5.1,message=_msg)))
            
            #define __availability_internal__iphone_4_0_dep__iphone_6_0   __attribute__((availability(ios,introduced=4.0,deprecated=6.0)))
            
                #define __availability_internal__iphone_4_0_dep__iphone_6_0_msg(_msg)   __attribute__((availability(ios,introduced=4.0,deprecated=6.0,message=_msg)))
            
            #define __availability_internal__iphone_4_0_dep__iphone_6_1   __attribute__((availability(ios,introduced=4.0,deprecated=6.1)))
            
                #define __availability_internal__iphone_4_0_dep__iphone_6_1_msg(_msg)   __attribute__((availability(ios,introduced=4.0,deprecated=6.1,message=_msg)))
            
            #define __availability_internal__iphone_4_0_dep__iphone_7_0   __attribute__((availability(ios,introduced=4.0,deprecated=7.0)))
            
                #define __availability_internal__iphone_4_0_dep__iphone_7_0_msg(_msg)   __attribute__((availability(ios,introduced=4.0,deprecated=7.0,message=_msg)))
            
            #define __availability_internal__iphone_4_0_dep__iphone_7_1   __attribute__((availability(ios,introduced=4.0,deprecated=7.1)))
            
                #define __availability_internal__iphone_4_0_dep__iphone_7_1_msg(_msg)   __attribute__((availability(ios,introduced=4.0,deprecated=7.1,message=_msg)))
            
            #define __availability_internal__iphone_4_0_dep__iphone_8_0   __attribute__((availability(ios,introduced=4.0,deprecated=8.0)))
            
                #define __availability_internal__iphone_4_0_dep__iphone_8_0_msg(_msg)   __attribute__((availability(ios,introduced=4.0,deprecated=8.0,message=_msg)))
            
            #define __availability_internal__iphone_4_0_dep__iphone_8_1   __attribute__((availability(ios,introduced=4.0,deprecated=8.1)))
            
                #define __availability_internal__iphone_4_0_dep__iphone_8_1_msg(_msg)   __attribute__((availability(ios,introduced=4.0,deprecated=8.1,message=_msg)))
            
            #define __availability_internal__iphone_4_0_dep__iphone_8_2   __attribute__((availability(ios,introduced=4.0,deprecated=8.2)))
            
                #define __availability_internal__iphone_4_0_dep__iphone_8_2_msg(_msg)   __attribute__((availability(ios,introduced=4.0,deprecated=8.2,message=_msg)))
            
            #define __availability_internal__iphone_4_0_dep__iphone_8_3   __attribute__((availability(ios,introduced=4.0,deprecated=8.3)))
            
                #define __availability_internal__iphone_4_0_dep__iphone_8_3_msg(_msg)   __attribute__((availability(ios,introduced=4.0,deprecated=8.3,message=_msg)))
            
            #define __availability_internal__iphone_4_0_dep__iphone_8_4   __attribute__((availability(ios,introduced=4.0,deprecated=8.4)))
            
                #define __availability_internal__iphone_4_0_dep__iphone_8_4_msg(_msg)   __attribute__((availability(ios,introduced=4.0,deprecated=8.4,message=_msg)))
            
            #define __availability_internal__iphone_4_0_dep__iphone_9_0   __attribute__((availability(ios,introduced=4.0,deprecated=9.0)))
            
                #define __availability_internal__iphone_4_0_dep__iphone_9_0_msg(_msg)   __attribute__((availability(ios,introduced=4.0,deprecated=9.0,message=_msg)))
            
            #define __availability_internal__iphone_4_0_dep__iphone_9_1   __attribute__((availability(ios,introduced=4.0,deprecated=9.1)))
            
                #define __availability_internal__iphone_4_0_dep__iphone_9_1_msg(_msg)   __attribute__((availability(ios,introduced=4.0,deprecated=9.1,message=_msg)))
            
            #define __availability_internal__iphone_4_0_dep__iphone_9_2   __attribute__((availability(ios,introduced=4.0,deprecated=9.2)))
            
                #define __availability_internal__iphone_4_0_dep__iphone_9_2_msg(_msg)   __attribute__((availability(ios,introduced=4.0,deprecated=9.2,message=_msg)))
            
            #define __availability_internal__iphone_4_0_dep__iphone_9_3   __attribute__((availability(ios,introduced=4.0,deprecated=9.3)))
            
                #define __availability_internal__iphone_4_0_dep__iphone_9_3_msg(_msg)   __attribute__((availability(ios,introduced=4.0,deprecated=9.3,message=_msg)))
            
            #define __availability_internal__iphone_4_0_dep__iphone_na   __attribute__((availability(ios,introduced=4.0)))
            
            #define __availability_internal__iphone_4_1                    __attribute__((availability(ios,introduced=4.1)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__iphone_4_1_dep__iphone_na_msg(_msg)   __attribute__((availability(ios,introduced=4.1)))
            
            #define __availability_internal__iphone_4_2_dep__iphone_10_0   __attribute__((availability(ios,introduced=4.2,deprecated=10.0)))
            
                #define __availability_internal__iphone_4_2_dep__iphone_10_0_msg(_msg)   __attribute__((availability(ios,introduced=4.2,deprecated=10.0,message=_msg)))
            
            #define __availability_internal__iphone_4_2_dep__iphone_10_1   __attribute__((availability(ios,introduced=4.2,deprecated=10.1)))
            
                #define __availability_internal__iphone_4_2_dep__iphone_10_1_msg(_msg)   __attribute__((availability(ios,introduced=4.2,deprecated=10.1,message=_msg)))
            
            #define __availability_internal__iphone_4_2_dep__iphone_10_2   __attribute__((availability(ios,introduced=4.2,deprecated=10.2)))
            
                #define __availability_internal__iphone_4_2_dep__iphone_10_2_msg(_msg)   __attribute__((availability(ios,introduced=4.2,deprecated=10.2,message=_msg)))
            
            #define __availability_internal__iphone_4_2_dep__iphone_10_3   __attribute__((availability(ios,introduced=4.2,deprecated=10.3)))
            
                #define __availability_internal__iphone_4_2_dep__iphone_10_3_msg(_msg)   __attribute__((availability(ios,introduced=4.2,deprecated=10.3,message=_msg)))
            
            #define __availability_internal__iphone_4_2_dep__iphone_4_2   __attribute__((availability(ios,introduced=4.2,deprecated=4.2)))
            
                #define __availability_internal__iphone_4_2_dep__iphone_4_2_msg(_msg)   __attribute__((availability(ios,introduced=4.2,deprecated=4.2,message=_msg)))
            
            #define __availability_internal__iphone_4_2_dep__iphone_4_3   __attribute__((availability(ios,introduced=4.2,deprecated=4.3)))
            
                #define __availability_internal__iphone_4_2_dep__iphone_4_3_msg(_msg)   __attribute__((availability(ios,introduced=4.2,deprecated=4.3,message=_msg)))
            
            #define __availability_internal__iphone_4_2_dep__iphone_5_0   __attribute__((availability(ios,introduced=4.2,deprecated=5.0)))
            
                #define __availability_internal__iphone_4_2_dep__iphone_5_0_msg(_msg)   __attribute__((availability(ios,introduced=4.2,deprecated=5.0,message=_msg)))
            
            #define __availability_internal__iphone_4_2_dep__iphone_5_1   __attribute__((availability(ios,introduced=4.2,deprecated=5.1)))
            
                #define __availability_internal__iphone_4_2_dep__iphone_5_1_msg(_msg)   __attribute__((availability(ios,introduced=4.2,deprecated=5.1,message=_msg)))
            
            #define __availability_internal__iphone_4_2_dep__iphone_6_0   __attribute__((availability(ios,introduced=4.2,deprecated=6.0)))
            
                #define __availability_internal__iphone_4_2_dep__iphone_6_0_msg(_msg)   __attribute__((availability(ios,introduced=4.2,deprecated=6.0,message=_msg)))
            
            #define __availability_internal__iphone_4_2_dep__iphone_6_1   __attribute__((availability(ios,introduced=4.2,deprecated=6.1)))
            
                #define __availability_internal__iphone_4_2_dep__iphone_6_1_msg(_msg)   __attribute__((availability(ios,introduced=4.2,deprecated=6.1,message=_msg)))
            
            #define __availability_internal__iphone_4_2_dep__iphone_7_0   __attribute__((availability(ios,introduced=4.2,deprecated=7.0)))
            
                #define __availability_internal__iphone_4_2_dep__iphone_7_0_msg(_msg)   __attribute__((availability(ios,introduced=4.2,deprecated=7.0,message=_msg)))
            
            #define __availability_internal__iphone_4_2_dep__iphone_7_1   __attribute__((availability(ios,introduced=4.2,deprecated=7.1)))
            
                #define __availability_internal__iphone_4_2_dep__iphone_7_1_msg(_msg)   __attribute__((availability(ios,introduced=4.2,deprecated=7.1,message=_msg)))
            
            #define __availability_internal__iphone_4_2_dep__iphone_8_0   __attribute__((availability(ios,introduced=4.2,deprecated=8.0)))
            
                #define __availability_internal__iphone_4_2_dep__iphone_8_0_msg(_msg)   __attribute__((availability(ios,introduced=4.2,deprecated=8.0,message=_msg)))
            
            #define __availability_internal__iphone_4_2_dep__iphone_8_1   __attribute__((availability(ios,introduced=4.2,deprecated=8.1)))
            
                #define __availability_internal__iphone_4_2_dep__iphone_8_1_msg(_msg)   __attribute__((availability(ios,introduced=4.2,deprecated=8.1,message=_msg)))
            
            #define __availability_internal__iphone_4_2_dep__iphone_8_2   __attribute__((availability(ios,introduced=4.2,deprecated=8.2)))
            
                #define __availability_internal__iphone_4_2_dep__iphone_8_2_msg(_msg)   __attribute__((availability(ios,introduced=4.2,deprecated=8.2,message=_msg)))
            
            #define __availability_internal__iphone_4_2_dep__iphone_8_3   __attribute__((availability(ios,introduced=4.2,deprecated=8.3)))
            
                #define __availability_internal__iphone_4_2_dep__iphone_8_3_msg(_msg)   __attribute__((availability(ios,introduced=4.2,deprecated=8.3,message=_msg)))
            
            #define __availability_internal__iphone_4_2_dep__iphone_8_4   __attribute__((availability(ios,introduced=4.2,deprecated=8.4)))
            
                #define __availability_internal__iphone_4_2_dep__iphone_8_4_msg(_msg)   __attribute__((availability(ios,introduced=4.2,deprecated=8.4,message=_msg)))
            
            #define __availability_internal__iphone_4_2_dep__iphone_9_0   __attribute__((availability(ios,introduced=4.2,deprecated=9.0)))
            
                #define __availability_internal__iphone_4_2_dep__iphone_9_0_msg(_msg)   __attribute__((availability(ios,introduced=4.2,deprecated=9.0,message=_msg)))
            
            #define __availability_internal__iphone_4_2_dep__iphone_9_1   __attribute__((availability(ios,introduced=4.2,deprecated=9.1)))
            
                #define __availability_internal__iphone_4_2_dep__iphone_9_1_msg(_msg)   __attribute__((availability(ios,introduced=4.2,deprecated=9.1,message=_msg)))
            
            #define __availability_internal__iphone_4_2_dep__iphone_9_2   __attribute__((availability(ios,introduced=4.2,deprecated=9.2)))
            
                #define __availability_internal__iphone_4_2_dep__iphone_9_2_msg(_msg)   __attribute__((availability(ios,introduced=4.2,deprecated=9.2,message=_msg)))
            
            #define __availability_internal__iphone_4_2_dep__iphone_9_3   __attribute__((availability(ios,introduced=4.2,deprecated=9.3)))
            
                #define __availability_internal__iphone_4_2_dep__iphone_9_3_msg(_msg)   __attribute__((availability(ios,introduced=4.2,deprecated=9.3,message=_msg)))
            
            #define __availability_internal__iphone_4_2_dep__iphone_na   __attribute__((availability(ios,introduced=4.2)))
            
            #define __availability_internal__iphone_4_3                    __attribute__((availability(ios,introduced=4.3)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__iphone_4_3_dep__iphone_na_msg(_msg)   __attribute__((availability(ios,introduced=4.3)))
            
            #define __availability_internal__iphone_5_0_dep__iphone_10_0   __attribute__((availability(ios,introduced=5.0,deprecated=10.0)))
            
                #define __availability_internal__iphone_5_0_dep__iphone_10_0_msg(_msg)   __attribute__((availability(ios,introduced=5.0,deprecated=10.0,message=_msg)))
            
            #define __availability_internal__iphone_5_0_dep__iphone_10_1   __attribute__((availability(ios,introduced=5.0,deprecated=10.1)))
            
                #define __availability_internal__iphone_5_0_dep__iphone_10_1_msg(_msg)   __attribute__((availability(ios,introduced=5.0,deprecated=10.1,message=_msg)))
            
            #define __availability_internal__iphone_5_0_dep__iphone_10_2   __attribute__((availability(ios,introduced=5.0,deprecated=10.2)))
            
                #define __availability_internal__iphone_5_0_dep__iphone_10_2_msg(_msg)   __attribute__((availability(ios,introduced=5.0,deprecated=10.2,message=_msg)))
            
            #define __availability_internal__iphone_5_0_dep__iphone_10_3   __attribute__((availability(ios,introduced=5.0,deprecated=10.3)))
            
                #define __availability_internal__iphone_5_0_dep__iphone_10_3_msg(_msg)   __attribute__((availability(ios,introduced=5.0,deprecated=10.3,message=_msg)))
            
            #define __availability_internal__iphone_5_0_dep__iphone_11_0   __attribute__((availability(ios,introduced=5.0,deprecated=11.0)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__iphone_5_0_dep__iphone_na_msg(_msg)   __attribute__((availability(ios,introduced=5.0)))
            
            #define __availability_internal__iphone_5_1_dep__iphone_10_0   __attribute__((availability(ios,introduced=5.1,deprecated=10.0)))
            
                #define __availability_internal__iphone_5_1_dep__iphone_10_0_msg(_msg)   __attribute__((availability(ios,introduced=5.1,deprecated=10.0,message=_msg)))
            
            #define __availability_internal__iphone_5_1_dep__iphone_10_1   __attribute__((availability(ios,introduced=5.1,deprecated=10.1)))
            
                #define __availability_internal__iphone_5_1_dep__iphone_10_1_msg(_msg)   __attribute__((availability(ios,introduced=5.1,deprecated=10.1,message=_msg)))
            
            #define __availability_internal__iphone_5_1_dep__iphone_10_2   __attribute__((availability(ios,introduced=5.1,deprecated=10.2)))
            
                #define __availability_internal__iphone_5_1_dep__iphone_10_2_msg(_msg)   __attribute__((availability(ios,introduced=5.1,deprecated=10.2,message=_msg)))
            
            #define __availability_internal__iphone_5_1_dep__iphone_10_3   __attribute__((availability(ios,introduced=5.1,deprecated=10.3)))
            
                #define __availability_internal__iphone_5_1_dep__iphone_10_3_msg(_msg)   __attribute__((availability(ios,introduced=5.1,deprecated=10.3,message=_msg)))
            
            #define __availability_internal__iphone_5_1_dep__iphone_5_1   __attribute__((availability(ios,introduced=5.1,deprecated=5.1)))
            
                #define __availability_internal__iphone_5_1_dep__iphone_5_1_msg(_msg)   __attribute__((availability(ios,introduced=5.1,deprecated=5.1,message=_msg)))
            
            #define __availability_internal__iphone_5_1_dep__iphone_6_0   __attribute__((availability(ios,introduced=5.1,deprecated=6.0)))
            
                #define __availability_internal__iphone_5_1_dep__iphone_6_0_msg(_msg)   __attribute__((availability(ios,introduced=5.1,deprecated=6.0,message=_msg)))
            
            #define __availability_internal__iphone_5_1_dep__iphone_6_1   __attribute__((availability(ios,introduced=5.1,deprecated=6.1)))
            
                #define __availability_internal__iphone_5_1_dep__iphone_6_1_msg(_msg)   __attribute__((availability(ios,introduced=5.1,deprecated=6.1,message=_msg)))
            
            #define __availability_internal__iphone_5_1_dep__iphone_7_0   __attribute__((availability(ios,introduced=5.1,deprecated=7.0)))
            
                #define __availability_internal__iphone_5_1_dep__iphone_7_0_msg(_msg)   __attribute__((availability(ios,introduced=5.1,deprecated=7.0,message=_msg)))
            
            #define __availability_internal__iphone_5_1_dep__iphone_7_1   __attribute__((availability(ios,introduced=5.1,deprecated=7.1)))
            
                #define __availability_internal__iphone_5_1_dep__iphone_7_1_msg(_msg)   __attribute__((availability(ios,introduced=5.1,deprecated=7.1,message=_msg)))
            
            #define __availability_internal__iphone_5_1_dep__iphone_8_0   __attribute__((availability(ios,introduced=5.1,deprecated=8.0)))
            
                #define __availability_internal__iphone_5_1_dep__iphone_8_0_msg(_msg)   __attribute__((availability(ios,introduced=5.1,deprecated=8.0,message=_msg)))
            
            #define __availability_internal__iphone_5_1_dep__iphone_8_1   __attribute__((availability(ios,introduced=5.1,deprecated=8.1)))
            
                #define __availability_internal__iphone_5_1_dep__iphone_8_1_msg(_msg)   __attribute__((availability(ios,introduced=5.1,deprecated=8.1,message=_msg)))
            
            #define __availability_internal__iphone_5_1_dep__iphone_8_2   __attribute__((availability(ios,introduced=5.1,deprecated=8.2)))
            
                #define __availability_internal__iphone_5_1_dep__iphone_8_2_msg(_msg)   __attribute__((availability(ios,introduced=5.1,deprecated=8.2,message=_msg)))
            
            #define __availability_internal__iphone_5_1_dep__iphone_8_3   __attribute__((availability(ios,introduced=5.1,deprecated=8.3)))
            
                #define __availability_internal__iphone_5_1_dep__iphone_8_3_msg(_msg)   __attribute__((availability(ios,introduced=5.1,deprecated=8.3,message=_msg)))
            
            #define __availability_internal__iphone_5_1_dep__iphone_8_4   __attribute__((availability(ios,introduced=5.1,deprecated=8.4)))
            
                #define __availability_internal__iphone_5_1_dep__iphone_8_4_msg(_msg)   __attribute__((availability(ios,introduced=5.1,deprecated=8.4,message=_msg)))
            
            #define __availability_internal__iphone_5_1_dep__iphone_9_0   __attribute__((availability(ios,introduced=5.1,deprecated=9.0)))
            
                #define __availability_internal__iphone_5_1_dep__iphone_9_0_msg(_msg)   __attribute__((availability(ios,introduced=5.1,deprecated=9.0,message=_msg)))
            
            #define __availability_internal__iphone_5_1_dep__iphone_9_1   __attribute__((availability(ios,introduced=5.1,deprecated=9.1)))
            
                #define __availability_internal__iphone_5_1_dep__iphone_9_1_msg(_msg)   __attribute__((availability(ios,introduced=5.1,deprecated=9.1,message=_msg)))
            
            #define __availability_internal__iphone_5_1_dep__iphone_9_2   __attribute__((availability(ios,introduced=5.1,deprecated=9.2)))
            
                #define __availability_internal__iphone_5_1_dep__iphone_9_2_msg(_msg)   __attribute__((availability(ios,introduced=5.1,deprecated=9.2,message=_msg)))
            
            #define __availability_internal__iphone_5_1_dep__iphone_9_3   __attribute__((availability(ios,introduced=5.1,deprecated=9.3)))
            
                #define __availability_internal__iphone_5_1_dep__iphone_9_3_msg(_msg)   __attribute__((availability(ios,introduced=5.1,deprecated=9.3,message=_msg)))
            
            #define __availability_internal__iphone_5_1_dep__iphone_na   __attribute__((availability(ios,introduced=5.1)))
            
            #define __availability_internal__iphone_6_0                    __attribute__((availability(ios,introduced=6.0)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__iphone_6_0_dep__iphone_na_msg(_msg)   __attribute__((availability(ios,introduced=6.0)))
            
            #define __availability_internal__iphone_6_1_dep__iphone_10_0   __attribute__((availability(ios,introduced=6.1,deprecated=10.0)))
            
                #define __availability_internal__iphone_6_1_dep__iphone_10_0_msg(_msg)   __attribute__((availability(ios,introduced=6.1,deprecated=10.0,message=_msg)))
            
            #define __availability_internal__iphone_6_1_dep__iphone_10_1   __attribute__((availability(ios,introduced=6.1,deprecated=10.1)))
            
                #define __availability_internal__iphone_6_1_dep__iphone_10_1_msg(_msg)   __attribute__((availability(ios,introduced=6.1,deprecated=10.1,message=_msg)))
            
            #define __availability_internal__iphone_6_1_dep__iphone_10_2   __attribute__((availability(ios,introduced=6.1,deprecated=10.2)))
            
                #define __availability_internal__iphone_6_1_dep__iphone_10_2_msg(_msg)   __attribute__((availability(ios,introduced=6.1,deprecated=10.2,message=_msg)))
            
            #define __availability_internal__iphone_6_1_dep__iphone_10_3   __attribute__((availability(ios,introduced=6.1,deprecated=10.3)))
            
                #define __availability_internal__iphone_6_1_dep__iphone_10_3_msg(_msg)   __attribute__((availability(ios,introduced=6.1,deprecated=10.3,message=_msg)))
            
            #define __availability_internal__iphone_6_1_dep__iphone_6_1   __attribute__((availability(ios,introduced=6.1,deprecated=6.1)))
            
                #define __availability_internal__iphone_6_1_dep__iphone_6_1_msg(_msg)   __attribute__((availability(ios,introduced=6.1,deprecated=6.1,message=_msg)))
            
            #define __availability_internal__iphone_6_1_dep__iphone_7_0   __attribute__((availability(ios,introduced=6.1,deprecated=7.0)))
            
                #define __availability_internal__iphone_6_1_dep__iphone_7_0_msg(_msg)   __attribute__((availability(ios,introduced=6.1,deprecated=7.0,message=_msg)))
            
            #define __availability_internal__iphone_6_1_dep__iphone_7_1   __attribute__((availability(ios,introduced=6.1,deprecated=7.1)))
            
                #define __availability_internal__iphone_6_1_dep__iphone_7_1_msg(_msg)   __attribute__((availability(ios,introduced=6.1,deprecated=7.1,message=_msg)))
            
            #define __availability_internal__iphone_6_1_dep__iphone_8_0   __attribute__((availability(ios,introduced=6.1,deprecated=8.0)))
            
                #define __availability_internal__iphone_6_1_dep__iphone_8_0_msg(_msg)   __attribute__((availability(ios,introduced=6.1,deprecated=8.0,message=_msg)))
            
            #define __availability_internal__iphone_6_1_dep__iphone_8_1   __attribute__((availability(ios,introduced=6.1,deprecated=8.1)))
            
                #define __availability_internal__iphone_6_1_dep__iphone_8_1_msg(_msg)   __attribute__((availability(ios,introduced=6.1,deprecated=8.1,message=_msg)))
            
            #define __availability_internal__iphone_6_1_dep__iphone_8_2   __attribute__((availability(ios,introduced=6.1,deprecated=8.2)))
            
                #define __availability_internal__iphone_6_1_dep__iphone_8_2_msg(_msg)   __attribute__((availability(ios,introduced=6.1,deprecated=8.2,message=_msg)))
            
            #define __availability_internal__iphone_6_1_dep__iphone_8_3   __attribute__((availability(ios,introduced=6.1,deprecated=8.3)))
            
                #define __availability_internal__iphone_6_1_dep__iphone_8_3_msg(_msg)   __attribute__((availability(ios,introduced=6.1,deprecated=8.3,message=_msg)))
            
            #define __availability_internal__iphone_6_1_dep__iphone_8_4   __attribute__((availability(ios,introduced=6.1,deprecated=8.4)))
            
                #define __availability_internal__iphone_6_1_dep__iphone_8_4_msg(_msg)   __attribute__((availability(ios,introduced=6.1,deprecated=8.4,message=_msg)))
            
            #define __availability_internal__iphone_6_1_dep__iphone_9_0   __attribute__((availability(ios,introduced=6.1,deprecated=9.0)))
            
                #define __availability_internal__iphone_6_1_dep__iphone_9_0_msg(_msg)   __attribute__((availability(ios,introduced=6.1,deprecated=9.0,message=_msg)))
            
            #define __availability_internal__iphone_6_1_dep__iphone_9_1   __attribute__((availability(ios,introduced=6.1,deprecated=9.1)))
            
                #define __availability_internal__iphone_6_1_dep__iphone_9_1_msg(_msg)   __attribute__((availability(ios,introduced=6.1,deprecated=9.1,message=_msg)))
            
            #define __availability_internal__iphone_6_1_dep__iphone_9_2   __attribute__((availability(ios,introduced=6.1,deprecated=9.2)))
            
                #define __availability_internal__iphone_6_1_dep__iphone_9_2_msg(_msg)   __attribute__((availability(ios,introduced=6.1,deprecated=9.2,message=_msg)))
            
            #define __availability_internal__iphone_6_1_dep__iphone_9_3   __attribute__((availability(ios,introduced=6.1,deprecated=9.3)))
            
                #define __availability_internal__iphone_6_1_dep__iphone_9_3_msg(_msg)   __attribute__((availability(ios,introduced=6.1,deprecated=9.3,message=_msg)))
            
            #define __availability_internal__iphone_6_1_dep__iphone_na   __attribute__((availability(ios,introduced=6.1)))
            
            #define __availability_internal__iphone_7_0                    __attribute__((availability(ios,introduced=7.0)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__iphone_7_0_dep__iphone_11_3   __attribute__((availability(ios,introduced=7.0,deprecated=11.3)))
            
                #define __availability_internal__iphone_7_0_dep__iphone_12_0_msg(_msg)   __attribute__((availability(ios,introduced=7.0,deprecated=12.0,message=_msg)))
            
            #define __availability_internal__iphone_7_0_dep__iphone_7_0   __attribute__((availability(ios,introduced=7.0,deprecated=7.0)))
            
                #define __availability_internal__iphone_7_0_dep__iphone_7_0_msg(_msg)   __attribute__((availability(ios,introduced=7.0,deprecated=7.0,message=_msg)))
            
            #define __availability_internal__iphone_7_0_dep__iphone_7_1   __attribute__((availability(ios,introduced=7.0,deprecated=7.1)))
            
                #define __availability_internal__iphone_7_0_dep__iphone_7_1_msg(_msg)   __attribute__((availability(ios,introduced=7.0,deprecated=7.1,message=_msg)))
            
            #define __availability_internal__iphone_7_0_dep__iphone_8_0   __attribute__((availability(ios,introduced=7.0,deprecated=8.0)))
            
                #define __availability_internal__iphone_7_0_dep__iphone_8_0_msg(_msg)   __attribute__((availability(ios,introduced=7.0,deprecated=8.0,message=_msg)))
            
            #define __availability_internal__iphone_7_0_dep__iphone_8_1   __attribute__((availability(ios,introduced=7.0,deprecated=8.1)))
            
                #define __availability_internal__iphone_7_0_dep__iphone_8_1_msg(_msg)   __attribute__((availability(ios,introduced=7.0,deprecated=8.1,message=_msg)))
            
            #define __availability_internal__iphone_7_0_dep__iphone_8_2   __attribute__((availability(ios,introduced=7.0,deprecated=8.2)))
            
                #define __availability_internal__iphone_7_0_dep__iphone_8_2_msg(_msg)   __attribute__((availability(ios,introduced=7.0,deprecated=8.2,message=_msg)))
            
            #define __availability_internal__iphone_7_0_dep__iphone_8_3   __attribute__((availability(ios,introduced=7.0,deprecated=8.3)))
            
                #define __availability_internal__iphone_7_0_dep__iphone_8_3_msg(_msg)   __attribute__((availability(ios,introduced=7.0,deprecated=8.3,message=_msg)))
            
            #define __availability_internal__iphone_7_0_dep__iphone_8_4   __attribute__((availability(ios,introduced=7.0,deprecated=8.4)))
            
                #define __availability_internal__iphone_7_0_dep__iphone_8_4_msg(_msg)   __attribute__((availability(ios,introduced=7.0,deprecated=8.4,message=_msg)))
            
            #define __availability_internal__iphone_7_0_dep__iphone_9_0   __attribute__((availability(ios,introduced=7.0,deprecated=9.0)))
            
                #define __availability_internal__iphone_7_0_dep__iphone_9_0_msg(_msg)   __attribute__((availability(ios,introduced=7.0,deprecated=9.0,message=_msg)))
            
            #define __availability_internal__iphone_7_0_dep__iphone_9_1   __attribute__((availability(ios,introduced=7.0,deprecated=9.1)))
            
                #define __availability_internal__iphone_7_0_dep__iphone_9_1_msg(_msg)   __attribute__((availability(ios,introduced=7.0,deprecated=9.1,message=_msg)))
            
            #define __availability_internal__iphone_7_0_dep__iphone_9_2   __attribute__((availability(ios,introduced=7.0,deprecated=9.2)))
            
                #define __availability_internal__iphone_7_0_dep__iphone_9_2_msg(_msg)   __attribute__((availability(ios,introduced=7.0,deprecated=9.2,message=_msg)))
            
            #define __availability_internal__iphone_7_0_dep__iphone_9_3   __attribute__((availability(ios,introduced=7.0,deprecated=9.3)))
            
                #define __availability_internal__iphone_7_0_dep__iphone_9_3_msg(_msg)   __attribute__((availability(ios,introduced=7.0,deprecated=9.3,message=_msg)))
            
            #define __availability_internal__iphone_7_0_dep__iphone_na   __attribute__((availability(ios,introduced=7.0)))
            
            #define __availability_internal__iphone_7_1                    __attribute__((availability(ios,introduced=7.1)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__iphone_7_1_dep__iphone_na_msg(_msg)   __attribute__((availability(ios,introduced=7.1)))
            
            #define __availability_internal__iphone_8_0_dep__iphone_10_0   __attribute__((availability(ios,introduced=8.0,deprecated=10.0)))
            
                #define __availability_internal__iphone_8_0_dep__iphone_10_0_msg(_msg)   __attribute__((availability(ios,introduced=8.0,deprecated=10.0,message=_msg)))
            
            #define __availability_internal__iphone_8_0_dep__iphone_10_1   __attribute__((availability(ios,introduced=8.0,deprecated=10.1)))
            
                #define __availability_internal__iphone_8_0_dep__iphone_10_1_msg(_msg)   __attribute__((availability(ios,introduced=8.0,deprecated=10.1,message=_msg)))
            
            #define __availability_internal__iphone_8_0_dep__iphone_10_2   __attribute__((availability(ios,introduced=8.0,deprecated=10.2)))
            
                #define __availability_internal__iphone_8_0_dep__iphone_10_2_msg(_msg)   __attribute__((availability(ios,introduced=8.0,deprecated=10.2,message=_msg)))
            
            #define __availability_internal__iphone_8_0_dep__iphone_10_3   __attribute__((availability(ios,introduced=8.0,deprecated=10.3)))
            
                #define __availability_internal__iphone_8_0_dep__iphone_10_3_msg(_msg)   __attribute__((availability(ios,introduced=8.0,deprecated=10.3,message=_msg)))
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__iphone_8_0_dep__iphone_12_0   __attribute__((availability(ios,introduced=8.0,deprecated=12.0)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__iphone_8_0_dep__iphone_na_msg(_msg)   __attribute__((availability(ios,introduced=8.0)))
            
            #define __availability_internal__iphone_8_1_dep__iphone_10_0   __attribute__((availability(ios,introduced=8.1,deprecated=10.0)))
            
                #define __availability_internal__iphone_8_1_dep__iphone_10_0_msg(_msg)   __attribute__((availability(ios,introduced=8.1,deprecated=10.0,message=_msg)))
            
            #define __availability_internal__iphone_8_1_dep__iphone_10_1   __attribute__((availability(ios,introduced=8.1,deprecated=10.1)))
            
                #define __availability_internal__iphone_8_1_dep__iphone_10_1_msg(_msg)   __attribute__((availability(ios,introduced=8.1,deprecated=10.1,message=_msg)))
            
            #define __availability_internal__iphone_8_1_dep__iphone_10_2   __attribute__((availability(ios,introduced=8.1,deprecated=10.2)))
            
                #define __availability_internal__iphone_8_1_dep__iphone_10_2_msg(_msg)   __attribute__((availability(ios,introduced=8.1,deprecated=10.2,message=_msg)))
            
            #define __availability_internal__iphone_8_1_dep__iphone_10_3   __attribute__((availability(ios,introduced=8.1,deprecated=10.3)))
            
                #define __availability_internal__iphone_8_1_dep__iphone_10_3_msg(_msg)   __attribute__((availability(ios,introduced=8.1,deprecated=10.3,message=_msg)))
            
            #define __availability_internal__iphone_8_1_dep__iphone_8_1   __attribute__((availability(ios,introduced=8.1,deprecated=8.1)))
            
                #define __availability_internal__iphone_8_1_dep__iphone_8_1_msg(_msg)   __attribute__((availability(ios,introduced=8.1,deprecated=8.1,message=_msg)))
            
            #define __availability_internal__iphone_8_1_dep__iphone_8_2   __attribute__((availability(ios,introduced=8.1,deprecated=8.2)))
            
                #define __availability_internal__iphone_8_1_dep__iphone_8_2_msg(_msg)   __attribute__((availability(ios,introduced=8.1,deprecated=8.2,message=_msg)))
            
            #define __availability_internal__iphone_8_1_dep__iphone_8_3   __attribute__((availability(ios,introduced=8.1,deprecated=8.3)))
            
                #define __availability_internal__iphone_8_1_dep__iphone_8_3_msg(_msg)   __attribute__((availability(ios,introduced=8.1,deprecated=8.3,message=_msg)))
            
            #define __availability_internal__iphone_8_1_dep__iphone_8_4   __attribute__((availability(ios,introduced=8.1,deprecated=8.4)))
            
                #define __availability_internal__iphone_8_1_dep__iphone_8_4_msg(_msg)   __attribute__((availability(ios,introduced=8.1,deprecated=8.4,message=_msg)))
            
            #define __availability_internal__iphone_8_1_dep__iphone_9_0   __attribute__((availability(ios,introduced=8.1,deprecated=9.0)))
            
                #define __availability_internal__iphone_8_1_dep__iphone_9_0_msg(_msg)   __attribute__((availability(ios,introduced=8.1,deprecated=9.0,message=_msg)))
            
            #define __availability_internal__iphone_8_1_dep__iphone_9_1   __attribute__((availability(ios,introduced=8.1,deprecated=9.1)))
            
                #define __availability_internal__iphone_8_1_dep__iphone_9_1_msg(_msg)   __attribute__((availability(ios,introduced=8.1,deprecated=9.1,message=_msg)))
            
            #define __availability_internal__iphone_8_1_dep__iphone_9_2   __attribute__((availability(ios,introduced=8.1,deprecated=9.2)))
            
                #define __availability_internal__iphone_8_1_dep__iphone_9_2_msg(_msg)   __attribute__((availability(ios,introduced=8.1,deprecated=9.2,message=_msg)))
            
            #define __availability_internal__iphone_8_1_dep__iphone_9_3   __attribute__((availability(ios,introduced=8.1,deprecated=9.3)))
            
                #define __availability_internal__iphone_8_1_dep__iphone_9_3_msg(_msg)   __attribute__((availability(ios,introduced=8.1,deprecated=9.3,message=_msg)))
            
            #define __availability_internal__iphone_8_1_dep__iphone_na   __attribute__((availability(ios,introduced=8.1)))
            
            #define __availability_internal__iphone_8_2                    __attribute__((availability(ios,introduced=8.2)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__iphone_8_2_dep__iphone_na_msg(_msg)   __attribute__((availability(ios,introduced=8.2)))
            
            #define __availability_internal__iphone_8_3_dep__iphone_10_0   __attribute__((availability(ios,introduced=8.3,deprecated=10.0)))
            
                #define __availability_internal__iphone_8_3_dep__iphone_10_0_msg(_msg)   __attribute__((availability(ios,introduced=8.3,deprecated=10.0,message=_msg)))
            
            #define __availability_internal__iphone_8_3_dep__iphone_10_1   __attribute__((availability(ios,introduced=8.3,deprecated=10.1)))
            
                #define __availability_internal__iphone_8_3_dep__iphone_10_1_msg(_msg)   __attribute__((availability(ios,introduced=8.3,deprecated=10.1,message=_msg)))
            
            #define __availability_internal__iphone_8_3_dep__iphone_10_2   __attribute__((availability(ios,introduced=8.3,deprecated=10.2)))
            
                #define __availability_internal__iphone_8_3_dep__iphone_10_2_msg(_msg)   __attribute__((availability(ios,introduced=8.3,deprecated=10.2,message=_msg)))
            
            #define __availability_internal__iphone_8_3_dep__iphone_10_3   __attribute__((availability(ios,introduced=8.3,deprecated=10.3)))
            
                #define __availability_internal__iphone_8_3_dep__iphone_10_3_msg(_msg)   __attribute__((availability(ios,introduced=8.3,deprecated=10.3,message=_msg)))
            
            #define __availability_internal__iphone_8_3_dep__iphone_8_3   __attribute__((availability(ios,introduced=8.3,deprecated=8.3)))
            
                #define __availability_internal__iphone_8_3_dep__iphone_8_3_msg(_msg)   __attribute__((availability(ios,introduced=8.3,deprecated=8.3,message=_msg)))
            
            #define __availability_internal__iphone_8_3_dep__iphone_8_4   __attribute__((availability(ios,introduced=8.3,deprecated=8.4)))
            
                #define __availability_internal__iphone_8_3_dep__iphone_8_4_msg(_msg)   __attribute__((availability(ios,introduced=8.3,deprecated=8.4,message=_msg)))
            
            #define __availability_internal__iphone_8_3_dep__iphone_9_0   __attribute__((availability(ios,introduced=8.3,deprecated=9.0)))
            
                #define __availability_internal__iphone_8_3_dep__iphone_9_0_msg(_msg)   __attribute__((availability(ios,introduced=8.3,deprecated=9.0,message=_msg)))
            
            #define __availability_internal__iphone_8_3_dep__iphone_9_1   __attribute__((availability(ios,introduced=8.3,deprecated=9.1)))
            
                #define __availability_internal__iphone_8_3_dep__iphone_9_1_msg(_msg)   __attribute__((availability(ios,introduced=8.3,deprecated=9.1,message=_msg)))
            
            #define __availability_internal__iphone_8_3_dep__iphone_9_2   __attribute__((availability(ios,introduced=8.3,deprecated=9.2)))
            
                #define __availability_internal__iphone_8_3_dep__iphone_9_2_msg(_msg)   __attribute__((availability(ios,introduced=8.3,deprecated=9.2,message=_msg)))
            
            #define __availability_internal__iphone_8_3_dep__iphone_9_3   __attribute__((availability(ios,introduced=8.3,deprecated=9.3)))
            
                #define __availability_internal__iphone_8_3_dep__iphone_9_3_msg(_msg)   __attribute__((availability(ios,introduced=8.3,deprecated=9.3,message=_msg)))
            
            #define __availability_internal__iphone_8_3_dep__iphone_na   __attribute__((availability(ios,introduced=8.3)))
            
            #define __availability_internal__iphone_8_4                    __attribute__((availability(ios,introduced=8.4)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__iphone_8_4_dep__iphone_na_msg(_msg)   __attribute__((availability(ios,introduced=8.4)))
            
            #define __availability_internal__iphone_9_0_dep__iphone_10_0   __attribute__((availability(ios,introduced=9.0,deprecated=10.0)))
            
                #define __availability_internal__iphone_9_0_dep__iphone_10_0_msg(_msg)   __attribute__((availability(ios,introduced=9.0,deprecated=10.0,message=_msg)))
            
            #define __availability_internal__iphone_9_0_dep__iphone_10_1   __attribute__((availability(ios,introduced=9.0,deprecated=10.1)))
            
                #define __availability_internal__iphone_9_0_dep__iphone_10_1_msg(_msg)   __attribute__((availability(ios,introduced=9.0,deprecated=10.1,message=_msg)))
            
            #define __availability_internal__iphone_9_0_dep__iphone_10_2   __attribute__((availability(ios,introduced=9.0,deprecated=10.2)))
            
                #define __availability_internal__iphone_9_0_dep__iphone_10_2_msg(_msg)   __attribute__((availability(ios,introduced=9.0,deprecated=10.2,message=_msg)))
            
            #define __availability_internal__iphone_9_0_dep__iphone_10_3   __attribute__((availability(ios,introduced=9.0,deprecated=10.3)))
            
                #define __availability_internal__iphone_9_0_dep__iphone_10_3_msg(_msg)   __attribute__((availability(ios,introduced=9.0,deprecated=10.3,message=_msg)))
            
            #define __availability_internal__iphone_9_0_dep__iphone_9_0   __attribute__((availability(ios,introduced=9.0,deprecated=9.0)))
            
                #define __availability_internal__iphone_9_0_dep__iphone_9_0_msg(_msg)   __attribute__((availability(ios,introduced=9.0,deprecated=9.0,message=_msg)))
            
            #define __availability_internal__iphone_9_0_dep__iphone_9_1   __attribute__((availability(ios,introduced=9.0,deprecated=9.1)))
            
                #define __availability_internal__iphone_9_0_dep__iphone_9_1_msg(_msg)   __attribute__((availability(ios,introduced=9.0,deprecated=9.1,message=_msg)))
            
            #define __availability_internal__iphone_9_0_dep__iphone_9_2   __attribute__((availability(ios,introduced=9.0,deprecated=9.2)))
            
                #define __availability_internal__iphone_9_0_dep__iphone_9_2_msg(_msg)   __attribute__((availability(ios,introduced=9.0,deprecated=9.2,message=_msg)))
            
            #define __availability_internal__iphone_9_0_dep__iphone_9_3   __attribute__((availability(ios,introduced=9.0,deprecated=9.3)))
            
                #define __availability_internal__iphone_9_0_dep__iphone_9_3_msg(_msg)   __attribute__((availability(ios,introduced=9.0,deprecated=9.3,message=_msg)))
            
            #define __availability_internal__iphone_9_0_dep__iphone_na   __attribute__((availability(ios,introduced=9.0)))
            
            #define __availability_internal__iphone_9_1                    __attribute__((availability(ios,introduced=9.1)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__iphone_9_1_dep__iphone_na_msg(_msg)   __attribute__((availability(ios,introduced=9.1)))
            
            #define __availability_internal__iphone_9_2_dep__iphone_10_0   __attribute__((availability(ios,introduced=9.2,deprecated=10.0)))
            
                #define __availability_internal__iphone_9_2_dep__iphone_10_0_msg(_msg)   __attribute__((availability(ios,introduced=9.2,deprecated=10.0,message=_msg)))
            
            #define __availability_internal__iphone_9_2_dep__iphone_10_1   __attribute__((availability(ios,introduced=9.2,deprecated=10.1)))
            
                #define __availability_internal__iphone_9_2_dep__iphone_10_1_msg(_msg)   __attribute__((availability(ios,introduced=9.2,deprecated=10.1,message=_msg)))
            
            #define __availability_internal__iphone_9_2_dep__iphone_10_2   __attribute__((availability(ios,introduced=9.2,deprecated=10.2)))
            
                #define __availability_internal__iphone_9_2_dep__iphone_10_2_msg(_msg)   __attribute__((availability(ios,introduced=9.2,deprecated=10.2,message=_msg)))
            
            #define __availability_internal__iphone_9_2_dep__iphone_10_3   __attribute__((availability(ios,introduced=9.2,deprecated=10.3)))
            
                #define __availability_internal__iphone_9_2_dep__iphone_10_3_msg(_msg)   __attribute__((availability(ios,introduced=9.2,deprecated=10.3,message=_msg)))
            
            #define __availability_internal__iphone_9_2_dep__iphone_9_2   __attribute__((availability(ios,introduced=9.2,deprecated=9.2)))
            
                #define __availability_internal__iphone_9_2_dep__iphone_9_2_msg(_msg)   __attribute__((availability(ios,introduced=9.2,deprecated=9.2,message=_msg)))
            
            #define __availability_internal__iphone_9_2_dep__iphone_9_3   __attribute__((availability(ios,introduced=9.2,deprecated=9.3)))
            
                #define __availability_internal__iphone_9_2_dep__iphone_9_3_msg(_msg)   __attribute__((availability(ios,introduced=9.2,deprecated=9.3,message=_msg)))
            
            #define __availability_internal__iphone_9_2_dep__iphone_na   __attribute__((availability(ios,introduced=9.2)))
            
            #define __availability_internal__iphone_9_3                    __attribute__((availability(ios,introduced=9.3)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__iphone_9_3_dep__iphone_na_msg(_msg)   __attribute__((availability(ios,introduced=9.3)))
            
            #define __availability_internal__iphone_10_0_dep__iphone_10_0   __attribute__((availability(ios,introduced=10.0,deprecated=10.0)))
            
                #define __availability_internal__iphone_10_0_dep__iphone_10_0_msg(_msg)   __attribute__((availability(ios,introduced=10.0,deprecated=10.0,message=_msg)))
            
            #define __availability_internal__iphone_10_0_dep__iphone_10_1   __attribute__((availability(ios,introduced=10.0,deprecated=10.1)))
            
                #define __availability_internal__iphone_10_0_dep__iphone_10_1_msg(_msg)   __attribute__((availability(ios,introduced=10.0,deprecated=10.1,message=_msg)))
            
            #define __availability_internal__iphone_10_0_dep__iphone_10_2   __attribute__((availability(ios,introduced=10.0,deprecated=10.2)))
            
                #define __availability_internal__iphone_10_0_dep__iphone_10_2_msg(_msg)   __attribute__((availability(ios,introduced=10.0,deprecated=10.2,message=_msg)))
            
            #define __availability_internal__iphone_10_0_dep__iphone_10_3   __attribute__((availability(ios,introduced=10.0,deprecated=10.3)))
            
                #define __availability_internal__iphone_10_0_dep__iphone_10_3_msg(_msg)   __attribute__((availability(ios,introduced=10.0,deprecated=10.3,message=_msg)))
            
            #define __availability_internal__iphone_10_0_dep__iphone_11_0   __attribute__((availability(ios,introduced=10.0,deprecated=11.0)))
            
            #define __availability_internal__iphone_10_0_dep__iphone_na   __attribute__((availability(ios,introduced=10.0)))
            
            #define __availability_internal__iphone_10_1                    __attribute__((availability(ios,introduced=10.1)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__iphone_10_1_dep__iphone_na_msg(_msg)   __attribute__((availability(ios,introduced=10.1)))
            
            #define __availability_internal__iphone_10_2_dep__iphone_10_2   __attribute__((availability(ios,introduced=10.2,deprecated=10.2)))
            
                #define __availability_internal__iphone_10_2_dep__iphone_10_2_msg(_msg)   __attribute__((availability(ios,introduced=10.2,deprecated=10.2,message=_msg)))
            
            #define __availability_internal__iphone_10_2_dep__iphone_10_3   __attribute__((availability(ios,introduced=10.2,deprecated=10.3)))
            
                #define __availability_internal__iphone_10_2_dep__iphone_10_3_msg(_msg)   __attribute__((availability(ios,introduced=10.2,deprecated=10.3,message=_msg)))
            
            #define __availability_internal__iphone_10_2_dep__iphone_na   __attribute__((availability(ios,introduced=10.2)))
            
            #define __availability_internal__iphone_10_3                    __attribute__((availability(ios,introduced=10.3)))
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__iphone_10_3_dep__iphone_na_msg(_msg)   __attribute__((availability(ios,introduced=10.3)))
            
            #define __availability_internal__iphone_11_0                    __attribute__((availability(ios,introduced=11.0)))
            
            #define __availability_internal__iphone_12_0                    __attribute__((availability(ios,introduced=12.0)))
            
            
            #define __availability_internal__iphone_na__iphone_na           __attribute__((availability(ios,unavailable)))
            
            #define __availability_internal__iphone_na_dep__iphone_na_msg(_msg) __attribute__((availability(ios,unavailable)))
            
             #if 1
              
               #if 1
                
                 #if 1
                  
                 #endif 
                
               #endif 
              
             #endif 
            
            
                #define __availability_internal__iphone_compat_version __attribute__((availability(ios,introduced=4.0)))
                
                #if 1
                
                #else
                
                #endif
            
        #endif
    
                #define __availability_internal__mac_10_1_dep__mac_10_11_msg(_msg)   __attribute__((availability(macosx,introduced=10.1,deprecated=10.11)))
            
            #define __availability_internal__mac_10_1_dep__mac_10_12   __attribute__((availability(macosx,introduced=10.1,deprecated=10.12)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
                #define __availability_internal__mac_10_1_dep__mac_10_12_msg(_msg)   __attribute__((availability(macosx,introduced=10.1,deprecated=10.12,message=_msg)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_1_dep__mac_na_msg(_msg)   __attribute__((availability(macosx,introduced=10.1)))
            
            #define __availability_internal__mac_10_2_dep__mac_10_1   __attribute__((availability(macosx,introduced=10.2,deprecated=10.1)))
            
            #define __availability_internal__mac_10_2_dep__mac_10_10_2   __attribute__((availability(macosx,introduced=10.2,deprecated=10.10.2)))
            
                #define __availability_internal__mac_10_2_dep__mac_10_10_2_msg(_msg)   __attribute__((availability(macosx,introduced=10.2,deprecated=10.10.2,message=_msg)))
            
            #define __availability_internal__mac_10_2_dep__mac_10_10_3   __attribute__((availability(macosx,introduced=10.2,deprecated=10.10.3)))
            
                #define __availability_internal__mac_10_2_dep__mac_10_10_3_msg(_msg)   __attribute__((availability(macosx,introduced=10.2,deprecated=10.10.3,message=_msg)))
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_2_dep__mac_10_11_2   __attribute__((availability(macosx,introduced=10.2,deprecated=10.11.2)))
            
                #define __availability_internal__mac_10_2_dep__mac_10_11_2_msg(_msg)   __attribute__((availability(macosx,introduced=10.2,deprecated=10.11.2,message=_msg)))
            
            #define __availability_internal__mac_10_2_dep__mac_10_11_3   __attribute__((availability(macosx,introduced=10.2,deprecated=10.11.3)))
            
                #define __availability_internal__mac_10_2_dep__mac_10_11_3_msg(_msg)   __attribute__((availability(macosx,introduced=10.2,deprecated=10.11.3,message=_msg)))
            
            #define __availability_internal__mac_10_2_dep__mac_10_11_4   __attribute__((availability(macosx,introduced=10.2,deprecated=10.11.4)))
            
                #define __availability_internal__mac_10_2_dep__mac_10_11_4_msg(_msg)   __attribute__((availability(macosx,introduced=10.2,deprecated=10.11.4,message=_msg)))
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_2_dep__mac_10_12_1   __attribute__((availability(macosx,introduced=10.2,deprecated=10.12.1)))
            
                #define __availability_internal__mac_10_2_dep__mac_10_12_1_msg(_msg)   __attribute__((availability(macosx,introduced=10.2,deprecated=10.12.1,message=_msg)))
            
            #define __availability_internal__mac_10_2_dep__mac_10_12_2   __attribute__((availability(macosx,introduced=10.2,deprecated=10.12.2)))
            
                #define __availability_internal__mac_10_2_dep__mac_10_12_2_msg(_msg)   __attribute__((availability(macosx,introduced=10.2,deprecated=10.12.2,message=_msg)))
            
            #define __availability_internal__mac_10_2_dep__mac_10_12_4   __attribute__((availability(macosx,introduced=10.2,deprecated=10.12.4)))
            
                #define __availability_internal__mac_10_2_dep__mac_10_12_4_msg(_msg)   __attribute__((availability(macosx,introduced=10.2,deprecated=10.12.4,message=_msg)))
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_2_dep__mac_10_2   __attribute__((availability(macosx,introduced=10.2,deprecated=10.2)))
            
                #define __availability_internal__mac_10_2_dep__mac_10_2_msg(_msg)   __attribute__((availability(macosx,introduced=10.2,deprecated=10.2,message=_msg)))
            
            #define __availability_internal__mac_10_2_dep__mac_10_3   __attribute__((availability(macosx,introduced=10.2,deprecated=10.3)))
            
                #define __availability_internal__mac_10_2_dep__mac_10_3_msg(_msg)   __attribute__((availability(macosx,introduced=10.2,deprecated=10.3,message=_msg)))
            
            #define __availability_internal__mac_10_2_dep__mac_10_4   __attribute__((availability(macosx,introduced=10.2,deprecated=10.4)))
            
                #define __availability_internal__mac_10_2_dep__mac_10_4_msg(_msg)   __attribute__((availability(macosx,introduced=10.2,deprecated=10.4,message=_msg)))
            
            #define __availability_internal__mac_10_2_dep__mac_10_5   __attribute__((availability(macosx,introduced=10.2,deprecated=10.5)))
            
                #define __availability_internal__mac_10_2_dep__mac_10_5_msg(_msg)   __attribute__((availability(macosx,introduced=10.2,deprecated=10.5,message=_msg)))
            
            #define __availability_internal__mac_10_2_dep__mac_10_6   __attribute__((availability(macosx,introduced=10.2,deprecated=10.6)))
            
                #define __availability_internal__mac_10_2_dep__mac_10_6_msg(_msg)   __attribute__((availability(macosx,introduced=10.2,deprecated=10.6,message=_msg)))
            
            #define __availability_internal__mac_10_2_dep__mac_10_7   __attribute__((availability(macosx,introduced=10.2,deprecated=10.7)))
            
                #define __availability_internal__mac_10_2_dep__mac_10_7_msg(_msg)   __attribute__((availability(macosx,introduced=10.2,deprecated=10.7,message=_msg)))
            
            #define __availability_internal__mac_10_2_dep__mac_10_8   __attribute__((availability(macosx,introduced=10.2,deprecated=10.8)))
            
                #define __availability_internal__mac_10_2_dep__mac_10_8_msg(_msg)   __attribute__((availability(macosx,introduced=10.2,deprecated=10.8,message=_msg)))
            
            #define __availability_internal__mac_10_2_dep__mac_10_9   __attribute__((availability(macosx,introduced=10.2,deprecated=10.9)))
            
                #define __availability_internal__mac_10_2_dep__mac_10_9_msg(_msg)   __attribute__((availability(macosx,introduced=10.2,deprecated=10.9,message=_msg)))
            
            #define __availability_internal__mac_10_2_dep__mac_na   __attribute__((availability(macosx,introduced=10.2)))
            
            #define __availability_internal__mac_10_3                    __attribute__((availability(macosx,introduced=10.3)))
            
            #define __availability_internal__mac_10_3_dep__mac_10_10   __attribute__((availability(macosx,introduced=10.3,deprecated=10.10)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
                #define __availability_internal__mac_10_3_dep__mac_10_10_msg(_msg)   __attribute__((availability(macosx,introduced=10.3,deprecated=10.10,message=_msg)))
            
            #define __availability_internal__mac_10_3_dep__mac_10_11   __attribute__((availability(macosx,introduced=10.3,deprecated=10.11)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
                #define __availability_internal__mac_10_3_dep__mac_10_11_msg(_msg)   __attribute__((availability(macosx,introduced=10.3,deprecated=10.11,message=_msg)))
            
            #define __availability_internal__mac_10_3_dep__mac_10_12   __attribute__((availability(macosx,introduced=10.3,deprecated=10.12)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
                #define __availability_internal__mac_10_3_dep__mac_10_12_msg(_msg)   __attribute__((availability(macosx,introduced=10.3,deprecated=10.12,message=_msg)))
            
            #define __availability_internal__mac_10_3_dep__mac_10_13   __attribute__((availability(macosx,introduced=10.3,deprecated=10.13)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_3_dep__mac_na_msg(_msg)   __attribute__((availability(macosx,introduced=10.3)))
            
            #define __availability_internal__mac_10_4_dep__mac_10_1   __attribute__((availability(macosx,introduced=10.4,deprecated=10.1)))
            
            #define __availability_internal__mac_10_4_dep__mac_10_10_2   __attribute__((availability(macosx,introduced=10.4,deprecated=10.10.2)))
            
                #define __availability_internal__mac_10_4_dep__mac_10_10_2_msg(_msg)   __attribute__((availability(macosx,introduced=10.4,deprecated=10.10.2,message=_msg)))
            
            #define __availability_internal__mac_10_4_dep__mac_10_10_3   __attribute__((availability(macosx,introduced=10.4,deprecated=10.10.3)))
            
                #define __availability_internal__mac_10_4_dep__mac_10_10_3_msg(_msg)   __attribute__((availability(macosx,introduced=10.4,deprecated=10.10.3,message=_msg)))
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_4_dep__mac_10_11_2   __attribute__((availability(macosx,introduced=10.4,deprecated=10.11.2)))
            
                #define __availability_internal__mac_10_4_dep__mac_10_11_2_msg(_msg)   __attribute__((availability(macosx,introduced=10.4,deprecated=10.11.2,message=_msg)))
            
            #define __availability_internal__mac_10_4_dep__mac_10_11_3   __attribute__((availability(macosx,introduced=10.4,deprecated=10.11.3)))
            
                #define __availability_internal__mac_10_4_dep__mac_10_11_3_msg(_msg)   __attribute__((availability(macosx,introduced=10.4,deprecated=10.11.3,message=_msg)))
            
            #define __availability_internal__mac_10_4_dep__mac_10_11_4   __attribute__((availability(macosx,introduced=10.4,deprecated=10.11.4)))
            
                #define __availability_internal__mac_10_4_dep__mac_10_11_4_msg(_msg)   __attribute__((availability(macosx,introduced=10.4,deprecated=10.11.4,message=_msg)))
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_4_dep__mac_10_12_1   __attribute__((availability(macosx,introduced=10.4,deprecated=10.12.1)))
            
                #define __availability_internal__mac_10_4_dep__mac_10_12_1_msg(_msg)   __attribute__((availability(macosx,introduced=10.4,deprecated=10.12.1,message=_msg)))
            
            #define __availability_internal__mac_10_4_dep__mac_10_12_2   __attribute__((availability(macosx,introduced=10.4,deprecated=10.12.2)))
            
                #define __availability_internal__mac_10_4_dep__mac_10_12_2_msg(_msg)   __attribute__((availability(macosx,introduced=10.4,deprecated=10.12.2,message=_msg)))
            
            #define __availability_internal__mac_10_4_dep__mac_10_12_4   __attribute__((availability(macosx,introduced=10.4,deprecated=10.12.4)))
            
                #define __availability_internal__mac_10_4_dep__mac_10_12_4_msg(_msg)   __attribute__((availability(macosx,introduced=10.4,deprecated=10.12.4,message=_msg)))
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_4_dep__mac_10_4   __attribute__((availability(macosx,introduced=10.4,deprecated=10.4)))
            
                #define __availability_internal__mac_10_4_dep__mac_10_4_msg(_msg)   __attribute__((availability(macosx,introduced=10.4,deprecated=10.4,message=_msg)))
            
            #define __availability_internal__mac_10_4_dep__mac_10_5   __attribute__((availability(macosx,introduced=10.4,deprecated=10.5)))
            
                #define __availability_internal__mac_10_4_dep__mac_10_5_msg(_msg)   __attribute__((availability(macosx,introduced=10.4,deprecated=10.5,message=_msg)))
            
            #define __availability_internal__mac_10_4_dep__mac_10_6   __attribute__((availability(macosx,introduced=10.4,deprecated=10.6)))
            
                #define __availability_internal__mac_10_4_dep__mac_10_6_msg(_msg)   __attribute__((availability(macosx,introduced=10.4,deprecated=10.6,message=_msg)))
            
            #define __availability_internal__mac_10_4_dep__mac_10_7   __attribute__((availability(macosx,introduced=10.4,deprecated=10.7)))
            
                #define __availability_internal__mac_10_4_dep__mac_10_7_msg(_msg)   __attribute__((availability(macosx,introduced=10.4,deprecated=10.7,message=_msg)))
            
            #define __availability_internal__mac_10_4_dep__mac_10_8   __attribute__((availability(macosx,introduced=10.4,deprecated=10.8)))
            
                #define __availability_internal__mac_10_4_dep__mac_10_8_msg(_msg)   __attribute__((availability(macosx,introduced=10.4,deprecated=10.8,message=_msg)))
            
            #define __availability_internal__mac_10_4_dep__mac_10_9   __attribute__((availability(macosx,introduced=10.4,deprecated=10.9)))
            
                #define __availability_internal__mac_10_4_dep__mac_10_9_msg(_msg)   __attribute__((availability(macosx,introduced=10.4,deprecated=10.9,message=_msg)))
            
            #define __availability_internal__mac_10_4_dep__mac_na   __attribute__((availability(macosx,introduced=10.4)))
            
            #define __availability_internal__mac_10_5                    __attribute__((availability(macosx,introduced=10.5)))
            
            #define __availability_internal__mac_10_5_dep__mac_10_1   __attribute__((availability(macosx,introduced=10.5,deprecated=10.1)))
            
            #define __availability_internal__mac_10_5_dep__mac_10_10_2   __attribute__((availability(macosx,introduced=10.5,deprecated=10.10.2)))
            
                #define __availability_internal__mac_10_5_dep__mac_10_10_2_msg(_msg)   __attribute__((availability(macosx,introduced=10.5,deprecated=10.10.2,message=_msg)))
            
            #define __availability_internal__mac_10_5_dep__mac_10_10_3   __attribute__((availability(macosx,introduced=10.5,deprecated=10.10.3)))
            
                #define __availability_internal__mac_10_5_dep__mac_10_10_3_msg(_msg)   __attribute__((availability(macosx,introduced=10.5,deprecated=10.10.3,message=_msg)))
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_5_dep__mac_10_11_2   __attribute__((availability(macosx,introduced=10.5,deprecated=10.11.2)))
            
                #define __availability_internal__mac_10_5_dep__mac_10_11_2_msg(_msg)   __attribute__((availability(macosx,introduced=10.5,deprecated=10.11.2,message=_msg)))
            
            #define __availability_internal__mac_10_5_dep__mac_10_11_3   __attribute__((availability(macosx,introduced=10.5,deprecated=10.11.3)))
            
                #define __availability_internal__mac_10_5_dep__mac_10_11_3_msg(_msg)   __attribute__((availability(macosx,introduced=10.5,deprecated=10.11.3,message=_msg)))
            
            #define __availability_internal__mac_10_5_dep__mac_10_11_4   __attribute__((availability(macosx,introduced=10.5,deprecated=10.11.4)))
            
                #define __availability_internal__mac_10_5_dep__mac_10_11_4_msg(_msg)   __attribute__((availability(macosx,introduced=10.5,deprecated=10.11.4,message=_msg)))
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_5_dep__mac_10_12_1   __attribute__((availability(macosx,introduced=10.5,deprecated=10.12.1)))
            
                #define __availability_internal__mac_10_5_dep__mac_10_12_1_msg(_msg)   __attribute__((availability(macosx,introduced=10.5,deprecated=10.12.1,message=_msg)))
            
            #define __availability_internal__mac_10_5_dep__mac_10_12_2   __attribute__((availability(macosx,introduced=10.5,deprecated=10.12.2)))
            
                #define __availability_internal__mac_10_5_dep__mac_10_12_2_msg(_msg)   __attribute__((availability(macosx,introduced=10.5,deprecated=10.12.2,message=_msg)))
            
            #define __availability_internal__mac_10_5_dep__mac_10_12_4   __attribute__((availability(macosx,introduced=10.5,deprecated=10.12.4)))
            
                #define __availability_internal__mac_10_5_dep__mac_10_12_4_msg(_msg)   __attribute__((availability(macosx,introduced=10.5,deprecated=10.12.4,message=_msg)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_5_dep__mac_na_msg(_msg)   __attribute__((availability(macosx,introduced=10.5)))
            
            #define __availability_internal__mac_10_6_dep__mac_10_1   __attribute__((availability(macosx,introduced=10.6,deprecated=10.1)))
            
            #define __availability_internal__mac_10_6_dep__mac_10_10_2   __attribute__((availability(macosx,introduced=10.6,deprecated=10.10.2)))
            
                #define __availability_internal__mac_10_6_dep__mac_10_10_2_msg(_msg)   __attribute__((availability(macosx,introduced=10.6,deprecated=10.10.2,message=_msg)))
            
            #define __availability_internal__mac_10_6_dep__mac_10_10_3   __attribute__((availability(macosx,introduced=10.6,deprecated=10.10.3)))
            
                #define __availability_internal__mac_10_6_dep__mac_10_10_3_msg(_msg)   __attribute__((availability(macosx,introduced=10.6,deprecated=10.10.3,message=_msg)))
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_6_dep__mac_10_11_2   __attribute__((availability(macosx,introduced=10.6,deprecated=10.11.2)))
            
                #define __availability_internal__mac_10_6_dep__mac_10_11_2_msg(_msg)   __attribute__((availability(macosx,introduced=10.6,deprecated=10.11.2,message=_msg)))
            
            #define __availability_internal__mac_10_6_dep__mac_10_11_3   __attribute__((availability(macosx,introduced=10.6,deprecated=10.11.3)))
            
                #define __availability_internal__mac_10_6_dep__mac_10_11_3_msg(_msg)   __attribute__((availability(macosx,introduced=10.6,deprecated=10.11.3,message=_msg)))
            
            #define __availability_internal__mac_10_6_dep__mac_10_11_4   __attribute__((availability(macosx,introduced=10.6,deprecated=10.11.4)))
            
                #define __availability_internal__mac_10_6_dep__mac_10_11_4_msg(_msg)   __attribute__((availability(macosx,introduced=10.6,deprecated=10.11.4,message=_msg)))
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_6_dep__mac_10_12_1   __attribute__((availability(macosx,introduced=10.6,deprecated=10.12.1)))
            
                #define __availability_internal__mac_10_6_dep__mac_10_12_1_msg(_msg)   __attribute__((availability(macosx,introduced=10.6,deprecated=10.12.1,message=_msg)))
            
            #define __availability_internal__mac_10_6_dep__mac_10_12_2   __attribute__((availability(macosx,introduced=10.6,deprecated=10.12.2)))
            
                #define __availability_internal__mac_10_6_dep__mac_10_12_2_msg(_msg)   __attribute__((availability(macosx,introduced=10.6,deprecated=10.12.2,message=_msg)))
            
            #define __availability_internal__mac_10_6_dep__mac_10_12_4   __attribute__((availability(macosx,introduced=10.6,deprecated=10.12.4)))
            
                #define __availability_internal__mac_10_6_dep__mac_10_12_4_msg(_msg)   __attribute__((availability(macosx,introduced=10.6,deprecated=10.12.4,message=_msg)))
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_6_dep__mac_10_6   __attribute__((availability(macosx,introduced=10.6,deprecated=10.6)))
            
                #define __availability_internal__mac_10_6_dep__mac_10_6_msg(_msg)   __attribute__((availability(macosx,introduced=10.6,deprecated=10.6,message=_msg)))
            
            #define __availability_internal__mac_10_6_dep__mac_10_7   __attribute__((availability(macosx,introduced=10.6,deprecated=10.7)))
            
                #define __availability_internal__mac_10_6_dep__mac_10_7_msg(_msg)   __attribute__((availability(macosx,introduced=10.6,deprecated=10.7,message=_msg)))
            
            #define __availability_internal__mac_10_6_dep__mac_10_8   __attribute__((availability(macosx,introduced=10.6,deprecated=10.8)))
            
                #define __availability_internal__mac_10_6_dep__mac_10_8_msg(_msg)   __attribute__((availability(macosx,introduced=10.6,deprecated=10.8,message=_msg)))
            
            #define __availability_internal__mac_10_6_dep__mac_10_9   __attribute__((availability(macosx,introduced=10.6,deprecated=10.9)))
            
                #define __availability_internal__mac_10_6_dep__mac_10_9_msg(_msg)   __attribute__((availability(macosx,introduced=10.6,deprecated=10.9,message=_msg)))
            
            #define __availability_internal__mac_10_6_dep__mac_na   __attribute__((availability(macosx,introduced=10.6)))
            
            #define __availability_internal__mac_10_7                    __attribute__((availability(macosx,introduced=10.7)))
            
            #define __availability_internal__mac_10_7_dep__mac_10_10   __attribute__((availability(macosx,introduced=10.7,deprecated=10.10)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
                #define __availability_internal__mac_10_7_dep__mac_10_10_msg(_msg)   __attribute__((availability(macosx,introduced=10.7,deprecated=10.10,message=_msg)))
            
            #define __availability_internal__mac_10_7_dep__mac_10_11   __attribute__((availability(macosx,introduced=10.7,deprecated=10.11)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
                #define __availability_internal__mac_10_7_dep__mac_10_11_msg(_msg)   __attribute__((availability(macosx,introduced=10.7,deprecated=10.11,message=_msg)))
            
            #define __availability_internal__mac_10_7_dep__mac_10_12   __attribute__((availability(macosx,introduced=10.7,deprecated=10.12)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
                #define __availability_internal__mac_10_7_dep__mac_10_12_msg(_msg)   __attribute__((availability(macosx,introduced=10.7,deprecated=10.12,message=_msg)))
            
            #define __availability_internal__mac_10_7_dep__mac_10_13_2   __attribute__((availability(macosx,introduced=10.7,deprecated=10.13.2)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_7_dep__mac_na_msg(_msg)   __attribute__((availability(macosx,introduced=10.7)))
            
            #define __availability_internal__mac_10_8_dep__mac_10_1   __attribute__((availability(macosx,introduced=10.8,deprecated=10.1)))
            
            #define __availability_internal__mac_10_8_dep__mac_10_10_2   __attribute__((availability(macosx,introduced=10.8,deprecated=10.10.2)))
            
                #define __availability_internal__mac_10_8_dep__mac_10_10_2_msg(_msg)   __attribute__((availability(macosx,introduced=10.8,deprecated=10.10.2,message=_msg)))
            
            #define __availability_internal__mac_10_8_dep__mac_10_10_3   __attribute__((availability(macosx,introduced=10.8,deprecated=10.10.3)))
            
                #define __availability_internal__mac_10_8_dep__mac_10_10_3_msg(_msg)   __attribute__((availability(macosx,introduced=10.8,deprecated=10.10.3,message=_msg)))
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_8_dep__mac_10_11_2   __attribute__((availability(macosx,introduced=10.8,deprecated=10.11.2)))
            
                #define __availability_internal__mac_10_8_dep__mac_10_11_2_msg(_msg)   __attribute__((availability(macosx,introduced=10.8,deprecated=10.11.2,message=_msg)))
            
            #define __availability_internal__mac_10_8_dep__mac_10_11_3   __attribute__((availability(macosx,introduced=10.8,deprecated=10.11.3)))
            
                #define __availability_internal__mac_10_8_dep__mac_10_11_3_msg(_msg)   __attribute__((availability(macosx,introduced=10.8,deprecated=10.11.3,message=_msg)))
            
            #define __availability_internal__mac_10_8_dep__mac_10_11_4   __attribute__((availability(macosx,introduced=10.8,deprecated=10.11.4)))
            
                #define __availability_internal__mac_10_8_dep__mac_10_11_4_msg(_msg)   __attribute__((availability(macosx,introduced=10.8,deprecated=10.11.4,message=_msg)))
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_8_dep__mac_10_12_1   __attribute__((availability(macosx,introduced=10.8,deprecated=10.12.1)))
            
                #define __availability_internal__mac_10_8_dep__mac_10_12_1_msg(_msg)   __attribute__((availability(macosx,introduced=10.8,deprecated=10.12.1,message=_msg)))
            
            #define __availability_internal__mac_10_8_dep__mac_10_12_2   __attribute__((availability(macosx,introduced=10.8,deprecated=10.12.2)))
            
                #define __availability_internal__mac_10_8_dep__mac_10_12_2_msg(_msg)   __attribute__((availability(macosx,introduced=10.8,deprecated=10.12.2,message=_msg)))
            
            #define __availability_internal__mac_10_8_dep__mac_10_12_4   __attribute__((availability(macosx,introduced=10.8,deprecated=10.12.4)))
            
                #define __availability_internal__mac_10_8_dep__mac_10_12_4_msg(_msg)   __attribute__((availability(macosx,introduced=10.8,deprecated=10.12.4,message=_msg)))
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_8_dep__mac_10_8   __attribute__((availability(macosx,introduced=10.8,deprecated=10.8)))
            
                #define __availability_internal__mac_10_8_dep__mac_10_8_msg(_msg)   __attribute__((availability(macosx,introduced=10.8,deprecated=10.8,message=_msg)))
            
            #define __availability_internal__mac_10_8_dep__mac_10_9   __attribute__((availability(macosx,introduced=10.8,deprecated=10.9)))
            
                #define __availability_internal__mac_10_8_dep__mac_10_9_msg(_msg)   __attribute__((availability(macosx,introduced=10.8,deprecated=10.9,message=_msg)))
            
            #define __availability_internal__mac_10_8_dep__mac_na   __attribute__((availability(macosx,introduced=10.8)))
            
            #define __availability_internal__mac_10_9                    __attribute__((availability(macosx,introduced=10.9)))
            
            #define __availability_internal__mac_10_9_dep__mac_10_10   __attribute__((availability(macosx,introduced=10.9,deprecated=10.10)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
                #define __availability_internal__mac_10_9_dep__mac_10_10_msg(_msg)   __attribute__((availability(macosx,introduced=10.9,deprecated=10.10,message=_msg)))
            
            #define __availability_internal__mac_10_9_dep__mac_10_11   __attribute__((availability(macosx,introduced=10.9,deprecated=10.11)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
                #define __availability_internal__mac_10_9_dep__mac_10_11_msg(_msg)   __attribute__((availability(macosx,introduced=10.9,deprecated=10.11,message=_msg)))
            
            #define __availability_internal__mac_10_9_dep__mac_10_12   __attribute__((availability(macosx,introduced=10.9,deprecated=10.12)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
                #define __availability_internal__mac_10_9_dep__mac_10_12_msg(_msg)   __attribute__((availability(macosx,introduced=10.9,deprecated=10.12,message=_msg)))
            
            #define __availability_internal__mac_10_9_dep__mac_10_13   __attribute__((availability(macosx,introduced=10.9,deprecated=10.13)))
            
            #define __availability_internal__mac_10_9_dep__mac_10_9   __attribute__((availability(macosx,introduced=10.9,deprecated=10.9)))
            
                #define __availability_internal__mac_10_9_dep__mac_10_9_msg(_msg)   __attribute__((availability(macosx,introduced=10.9,deprecated=10.9,message=_msg)))
            
            #define __availability_internal__mac_10_9_dep__mac_na   __attribute__((availability(macosx,introduced=10.9)))
            
            #define __availability_internal__mac_10_0                    __attribute__((availability(macosx,introduced=10.0)))
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_0_dep__mac_10_10   __attribute__((availability(macosx,introduced=10.0,deprecated=10.10)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
                #define __availability_internal__mac_10_0_dep__mac_10_10_msg(_msg)   __attribute__((availability(macosx,introduced=10.0,deprecated=10.10,message=_msg)))
            
            #define __availability_internal__mac_10_0_dep__mac_10_11   __attribute__((availability(macosx,introduced=10.0,deprecated=10.11)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
                #define __availability_internal__mac_10_0_dep__mac_10_11_msg(_msg)   __attribute__((availability(macosx,introduced=10.0,deprecated=10.11,message=_msg)))
            
            #define __availability_internal__mac_10_0_dep__mac_10_12   __attribute__((availability(macosx,introduced=10.0,deprecated=10.12)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
                #define __availability_internal__mac_10_0_dep__mac_10_12_msg(_msg)   __attribute__((availability(macosx,introduced=10.0,deprecated=10.12,message=_msg)))
            
            #define __availability_internal__mac_10_0_dep__mac_10_13   __attribute__((availability(macosx,introduced=10.0,deprecated=10.13)))
            
                #define __availability_internal__mac_10_0_dep__mac_10_1_msg(_msg)   __attribute__((availability(macosx,introduced=10.0,deprecated=10.1,message=_msg)))
            
            #define __availability_internal__mac_10_0_dep__mac_10_2   __attribute__((availability(macosx,introduced=10.0,deprecated=10.2)))
            
                #define __availability_internal__mac_10_0_dep__mac_10_2_msg(_msg)   __attribute__((availability(macosx,introduced=10.0,deprecated=10.2,message=_msg)))
            
            #define __availability_internal__mac_10_0_dep__mac_10_3   __attribute__((availability(macosx,introduced=10.0,deprecated=10.3)))
            
                #define __availability_internal__mac_10_0_dep__mac_10_3_msg(_msg)   __attribute__((availability(macosx,introduced=10.0,deprecated=10.3,message=_msg)))
            
            #define __availability_internal__mac_10_0_dep__mac_10_4   __attribute__((availability(macosx,introduced=10.0,deprecated=10.4)))
            
                #define __availability_internal__mac_10_0_dep__mac_10_4_msg(_msg)   __attribute__((availability(macosx,introduced=10.0,deprecated=10.4,message=_msg)))
            
            #define __availability_internal__mac_10_0_dep__mac_10_5   __attribute__((availability(macosx,introduced=10.0,deprecated=10.5)))
            
                #define __availability_internal__mac_10_0_dep__mac_10_5_msg(_msg)   __attribute__((availability(macosx,introduced=10.0,deprecated=10.5,message=_msg)))
            
            #define __availability_internal__mac_10_0_dep__mac_10_6   __attribute__((availability(macosx,introduced=10.0,deprecated=10.6)))
            
                #define __availability_internal__mac_10_0_dep__mac_10_6_msg(_msg)   __attribute__((availability(macosx,introduced=10.0,deprecated=10.6,message=_msg)))
            
            #define __availability_internal__mac_10_0_dep__mac_10_7   __attribute__((availability(macosx,introduced=10.0,deprecated=10.7)))
            
                #define __availability_internal__mac_10_0_dep__mac_10_7_msg(_msg)   __attribute__((availability(macosx,introduced=10.0,deprecated=10.7,message=_msg)))
            
            #define __availability_internal__mac_10_0_dep__mac_10_8   __attribute__((availability(macosx,introduced=10.0,deprecated=10.8)))
            
                #define __availability_internal__mac_10_0_dep__mac_10_8_msg(_msg)   __attribute__((availability(macosx,introduced=10.0,deprecated=10.8,message=_msg)))
            
            #define __availability_internal__mac_10_0_dep__mac_10_9   __attribute__((availability(macosx,introduced=10.0,deprecated=10.9)))
            
                #define __availability_internal__mac_10_0_dep__mac_10_9_msg(_msg)   __attribute__((availability(macosx,introduced=10.0,deprecated=10.9,message=_msg)))
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_0_dep__mac_na_msg(_msg)   __attribute__((availability(macosx,introduced=10.0)))
            
            #define __availability_internal__mac_10_10                    __attribute__((availability(macosx,introduced=10.10)))
            
            #define __availability_internal__mac_10_10_2_dep__mac_10_10_2   __attribute__((availability(macosx,introduced=10.10.2,deprecated=10.10.2)))
            
                #define __availability_internal__mac_10_10_2_dep__mac_10_10_2_msg(_msg)   __attribute__((availability(macosx,introduced=10.10.2,deprecated=10.10.2,message=_msg)))
            
            #define __availability_internal__mac_10_10_2_dep__mac_10_10_3   __attribute__((availability(macosx,introduced=10.10.2,deprecated=10.10.3)))
            
                #define __availability_internal__mac_10_10_2_dep__mac_10_10_3_msg(_msg)   __attribute__((availability(macosx,introduced=10.10.2,deprecated=10.10.3,message=_msg)))
            
            #define __availability_internal__mac_10_10_2_dep__mac_10_11   __attribute__((availability(macosx,introduced=10.10.2,deprecated=10.11)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
                #define __availability_internal__mac_10_10_2_dep__mac_10_11_msg(_msg)   __attribute__((availability(macosx,introduced=10.10.2,deprecated=10.11,message=_msg)))
            
            #define __availability_internal__mac_10_10_2_dep__mac_10_12   __attribute__((availability(macosx,introduced=10.10.2,deprecated=10.12)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
                #define __availability_internal__mac_10_10_2_dep__mac_10_12_msg(_msg)   __attribute__((availability(macosx,introduced=10.10.2,deprecated=10.12,message=_msg)))
            
            #define __availability_internal__mac_10_10_2_dep__mac_na   __attribute__((availability(macosx,introduced=10.10.2)))
            
            #define __availability_internal__mac_10_10_3                    __attribute__((availability(macosx,introduced=10.10.3)))
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_10_3_dep__mac_10_11_2   __attribute__((availability(macosx,introduced=10.10.3,deprecated=10.11.2)))
            
                #define __availability_internal__mac_10_10_3_dep__mac_10_11_2_msg(_msg)   __attribute__((availability(macosx,introduced=10.10.3,deprecated=10.11.2,message=_msg)))
            
            #define __availability_internal__mac_10_10_3_dep__mac_10_11_3   __attribute__((availability(macosx,introduced=10.10.3,deprecated=10.11.3)))
            
                #define __availability_internal__mac_10_10_3_dep__mac_10_11_3_msg(_msg)   __attribute__((availability(macosx,introduced=10.10.3,deprecated=10.11.3,message=_msg)))
            
            #define __availability_internal__mac_10_10_3_dep__mac_10_11_4   __attribute__((availability(macosx,introduced=10.10.3,deprecated=10.11.4)))
            
                #define __availability_internal__mac_10_10_3_dep__mac_10_11_4_msg(_msg)   __attribute__((availability(macosx,introduced=10.10.3,deprecated=10.11.4,message=_msg)))
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_10_3_dep__mac_10_12_1   __attribute__((availability(macosx,introduced=10.10.3,deprecated=10.12.1)))
            
                #define __availability_internal__mac_10_10_3_dep__mac_10_12_1_msg(_msg)   __attribute__((availability(macosx,introduced=10.10.3,deprecated=10.12.1,message=_msg)))
            
            #define __availability_internal__mac_10_10_3_dep__mac_10_12_2   __attribute__((availability(macosx,introduced=10.10.3,deprecated=10.12.2)))
            
                #define __availability_internal__mac_10_10_3_dep__mac_10_12_2_msg(_msg)   __attribute__((availability(macosx,introduced=10.10.3,deprecated=10.12.2,message=_msg)))
            
            #define __availability_internal__mac_10_10_3_dep__mac_10_12_4   __attribute__((availability(macosx,introduced=10.10.3,deprecated=10.12.4)))
            
                #define __availability_internal__mac_10_10_3_dep__mac_10_12_4_msg(_msg)   __attribute__((availability(macosx,introduced=10.10.3,deprecated=10.12.4,message=_msg)))
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_10_3_dep__mac_na_msg(_msg)   __attribute__((availability(macosx,introduced=10.10.3)))
            
            #define __availability_internal__mac_10_10_dep__mac_10_10   __attribute__((availability(macosx,introduced=10.10,deprecated=10.10)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
                #define __availability_internal__mac_10_10_dep__mac_10_10_msg(_msg)   __attribute__((availability(macosx,introduced=10.10,deprecated=10.10,message=_msg)))
            
            #define __availability_internal__mac_10_10_dep__mac_10_11   __attribute__((availability(macosx,introduced=10.10,deprecated=10.11)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
                #define __availability_internal__mac_10_10_dep__mac_10_11_msg(_msg)   __attribute__((availability(macosx,introduced=10.10,deprecated=10.11,message=_msg)))
            
            #define __availability_internal__mac_10_10_dep__mac_10_12   __attribute__((availability(macosx,introduced=10.10,deprecated=10.12)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
                #define __availability_internal__mac_10_10_dep__mac_10_12_msg(_msg)   __attribute__((availability(macosx,introduced=10.10,deprecated=10.12,message=_msg)))
            
            #define __availability_internal__mac_10_10_dep__mac_10_13   __attribute__((availability(macosx,introduced=10.10,deprecated=10.13)))
            
                #define __availability_internal__mac_10_10_dep__mac_10_13_msg(_msg)   __attribute__((availability(macosx,introduced=10.10,deprecated=10.13,message=_msg)))
            
            #define __availability_internal__mac_10_10_dep__mac_10_13_4   __attribute__((availability(macosx,introduced=10.10,deprecated=10.13.4)))
            
            #define __availability_internal__mac_10_10_dep__mac_na_msg(_msg)   __attribute__((availability(macosx,introduced=10.10)))
            
            #define __availability_internal__mac_10_11_2                    __attribute__((availability(macosx,introduced=10.11.2)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_11_2_dep__mac_10_12_1   __attribute__((availability(macosx,introduced=10.11.2,deprecated=10.12.1)))
            
                #define __availability_internal__mac_10_11_2_dep__mac_10_12_1_msg(_msg)   __attribute__((availability(macosx,introduced=10.11.2,deprecated=10.12.1,message=_msg)))
            
            #define __availability_internal__mac_10_11_2_dep__mac_10_12_2   __attribute__((availability(macosx,introduced=10.11.2,deprecated=10.12.2)))
            
                #define __availability_internal__mac_10_11_2_dep__mac_10_12_2_msg(_msg)   __attribute__((availability(macosx,introduced=10.11.2,deprecated=10.12.2,message=_msg)))
            
            #define __availability_internal__mac_10_11_2_dep__mac_10_12_4   __attribute__((availability(macosx,introduced=10.11.2,deprecated=10.12.4)))
            
                #define __availability_internal__mac_10_11_2_dep__mac_10_12_4_msg(_msg)   __attribute__((availability(macosx,introduced=10.11.2,deprecated=10.12.4,message=_msg)))
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_11_2_dep__mac_na_msg(_msg)   __attribute__((availability(macosx,introduced=10.11.2)))
            
            #define __availability_internal__mac_10_11_3_dep__mac_10_11_3   __attribute__((availability(macosx,introduced=10.11.3,deprecated=10.11.3)))
            
                #define __availability_internal__mac_10_11_3_dep__mac_10_11_3_msg(_msg)   __attribute__((availability(macosx,introduced=10.11.3,deprecated=10.11.3,message=_msg)))
            
            #define __availability_internal__mac_10_11_3_dep__mac_10_11_4   __attribute__((availability(macosx,introduced=10.11.3,deprecated=10.11.4)))
            
                #define __availability_internal__mac_10_11_3_dep__mac_10_11_4_msg(_msg)   __attribute__((availability(macosx,introduced=10.11.3,deprecated=10.11.4,message=_msg)))
            
            #define __availability_internal__mac_10_11_3_dep__mac_10_12   __attribute__((availability(macosx,introduced=10.11.3,deprecated=10.12)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
                #define __availability_internal__mac_10_11_3_dep__mac_10_12_msg(_msg)   __attribute__((availability(macosx,introduced=10.11.3,deprecated=10.12,message=_msg)))
            
            #define __availability_internal__mac_10_11_3_dep__mac_na   __attribute__((availability(macosx,introduced=10.11.3)))
            
            #define __availability_internal__mac_10_11_4                    __attribute__((availability(macosx,introduced=10.11.4)))
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_11_4_dep__mac_10_12_1   __attribute__((availability(macosx,introduced=10.11.4,deprecated=10.12.1)))
            
                #define __availability_internal__mac_10_11_4_dep__mac_10_12_1_msg(_msg)   __attribute__((availability(macosx,introduced=10.11.4,deprecated=10.12.1,message=_msg)))
            
            #define __availability_internal__mac_10_11_4_dep__mac_10_12_2   __attribute__((availability(macosx,introduced=10.11.4,deprecated=10.12.2)))
            
                #define __availability_internal__mac_10_11_4_dep__mac_10_12_2_msg(_msg)   __attribute__((availability(macosx,introduced=10.11.4,deprecated=10.12.2,message=_msg)))
            
            #define __availability_internal__mac_10_11_4_dep__mac_10_12_4   __attribute__((availability(macosx,introduced=10.11.4,deprecated=10.12.4)))
            
                #define __availability_internal__mac_10_11_4_dep__mac_10_12_4_msg(_msg)   __attribute__((availability(macosx,introduced=10.11.4,deprecated=10.12.4,message=_msg)))
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_11_4_dep__mac_na_msg(_msg)   __attribute__((availability(macosx,introduced=10.11.4)))
            
            #define __availability_internal__mac_10_11_dep__mac_10_11   __attribute__((availability(macosx,introduced=10.11,deprecated=10.11)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
                #define __availability_internal__mac_10_11_dep__mac_10_11_msg(_msg)   __attribute__((availability(macosx,introduced=10.11,deprecated=10.11,message=_msg)))
            
            #define __availability_internal__mac_10_11_dep__mac_10_12   __attribute__((availability(macosx,introduced=10.11,deprecated=10.12)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
                #define __availability_internal__mac_10_11_dep__mac_10_12_msg(_msg)   __attribute__((availability(macosx,introduced=10.11,deprecated=10.12,message=_msg)))
            
            #define __availability_internal__mac_10_11_dep__mac_na   __attribute__((availability(macosx,introduced=10.11)))
            
            #define __availability_internal__mac_10_12                    __attribute__((availability(macosx,introduced=10.12)))
            
            #define __availability_internal__mac_10_12_1_dep__mac_10_12_1   __attribute__((availability(macosx,introduced=10.12.1,deprecated=10.12.1)))
            
                #define __availability_internal__mac_10_12_1_dep__mac_10_12_1_msg(_msg)   __attribute__((availability(macosx,introduced=10.12.1,deprecated=10.12.1,message=_msg)))
            
            #define __availability_internal__mac_10_12_1_dep__mac_10_12_2   __attribute__((availability(macosx,introduced=10.12.1,deprecated=10.12.2)))
            
                #define __availability_internal__mac_10_12_1_dep__mac_10_12_2_msg(_msg)   __attribute__((availability(macosx,introduced=10.12.1,deprecated=10.12.2,message=_msg)))
            
            #define __availability_internal__mac_10_12_1_dep__mac_10_12_4   __attribute__((availability(macosx,introduced=10.12.1,deprecated=10.12.4)))
            
                #define __availability_internal__mac_10_12_1_dep__mac_10_12_4_msg(_msg)   __attribute__((availability(macosx,introduced=10.12.1,deprecated=10.12.4,message=_msg)))
            
            #define __availability_internal__mac_10_12_1_dep__mac_na   __attribute__((availability(macosx,introduced=10.12.1)))
            
            #define __availability_internal__mac_10_12_2                    __attribute__((availability(macosx,introduced=10.12.2)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #define __availability_internal__mac_10_12_2_dep__mac_na_msg(_msg)   __attribute__((availability(macosx,introduced=10.12.2)))
            
            #define __availability_internal__mac_10_12_4_dep__mac_10_12_4   __attribute__((availability(macosx,introduced=10.12.4,deprecated=10.12.4)))
            
                #define __availability_internal__mac_10_12_4_dep__mac_10_12_4_msg(_msg)   __attribute__((availability(macosx,introduced=10.12.4,deprecated=10.12.4,message=_msg)))
            
            #define __availability_internal__mac_10_12_4_dep__mac_na   __attribute__((availability(macosx,introduced=10.12.4)))
            
            #define __availability_internal__mac_10_12_dep__mac_10_12   __attribute__((availability(macosx,introduced=10.12,deprecated=10.12)))
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
            #if 1
                
            #else
                
            #endif
            
                #define __availability_internal__mac_10_12_dep__mac_10_12_msg(_msg)   __attribute__((availability(macosx,introduced=10.12,deprecated=10.12,message=_msg)))
            
            #define __availability_internal__mac_10_12_dep__mac_10_13   __attribute__((availability(macosx,introduced=10.12,deprecated=10.13)))
            
                #define __availability_internal__mac_10_12_dep__mac_10_13_msg(_msg)   __attribute__((availability(macosx,introduced=10.12,deprecated=10.13,message=_msg)))
            
            #define __availability_internal__mac_10_12_dep__mac_10_13_4   __attribute__((availability(macosx,introduced=10.12,deprecated=10.13.4)))
            
            #define __availability_internal__mac_10_12_dep__mac_na   __attribute__((availability(macosx,introduced=10.12)))
            
            #define __availability_internal__mac_10_13                    __attribute__((availability(macosx,introduced=10.13)))
            
            #define __availability_internal__mac_10_14                    __attribute__((availability(macosx,introduced=10.14)))
            
            #define __availability_internal__mac_10_15                    __attribute__((availability(macosx,introduced=10.15)))
            
            #define __availability_internal__mac_na_dep__mac_na            __attribute__((availability(macosx,unavailable)))
            
            
            #define __availability_internal__iphone_na__iphone_na           __attribute__((availability(ios,unavailable)))
            
            #define __availability_internal__iphone_na_dep__iphone_na_msg(_msg) __attribute__((availability(ios,unavailable)))
            
             #define __availability_internal__iphone_compat_version                                          __attribute__((availability(ios,unavailable)))
             
             #define __attribute__((availability(ios,introduced=4.0,deprecated=4.0)))     __attribute__((availability(ios,unavailable)))
            
        #endif
    
 #if 1
   
   #define __api_deprecated_platform_macos(x,y) macos,introduced=x,deprecated=y
   
   #define __api_available_platform_macosx(x) macos,introduced=x
   
   #define __api_unavailable_platform_macosx macos,unavailable
   
   #define __api_deprecated_platform_ios(x,y) ios,introduced=x,deprecated=y
   
   #define __api_available_platform_maccatalyst(x) maccatalyst,introduced=x
   
   #define __api_unavailable_platform_maccatalyst maccatalyst,unavailable
   
   #define maccatalyst,introduced=x,deprecated=y maccatalyst,introduced=x,deprecated=y
   
   #define __api_available_platform_watchos(x) watchos,introduced=x
   
   #define __api_unavailable_platform_watchos watchos,unavailable
   
   #define __api_deprecated_platform_tvos(x,y) tvos,introduced=x,deprecated=y
   
   
   
   
   
   #define __api_deprecated_platform_driverkit(x,y) driverkit,introduced=x,deprecated=y
   
   #define __api_available_platform_visionos(x) visionos,introduced=x
   
   #define __api_unavailable_platform_visionos visionos,unavailable
   
   #define __api_deprecated_platform_xros(x,y) visionos,introduced=x,deprecated=y
   
   
 
 #if 1
  
  #define __api_range_stringify(x) __api_range_stringify2(x)
  
 #endif 
   
 #if 1
    
    
    
    
    #define __api_available1(arg0,arg1) __attribute__((availability(__api_available_platform_##arg0))) __attribute__((availability(__api_available_platform_##arg1)))
    
    #define __api_available3(arg0,arg1,arg2,arg3) __attribute__((availability(__api_available_platform_##arg0))) __attribute__((availability(__api_available_platform_##arg1))) __attribute__((availability(__api_available_platform_##arg2))) __attribute__((availability(__api_available_platform_##arg3)))
    
    #define __api_available5(arg0,arg1,arg2,arg3,arg4,arg5) __attribute__((availability(__api_available_platform_##arg0))) __attribute__((availability(__api_available_platform_##arg1))) __attribute__((availability(__api_available_platform_##arg2))) __attribute__((availability(__api_available_platform_##arg3))) __attribute__((availability(__api_available_platform_##arg4))) __attribute__((availability(__api_available_platform_##arg5)))
    
    #define __api_available7(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7) __attribute__((availability(__api_available_platform_##arg0))) __attribute__((availability(__api_available_platform_##arg1))) __attribute__((availability(__api_available_platform_##arg2))) __attribute__((availability(__api_available_platform_##arg3))) __attribute__((availability(__api_available_platform_##arg4))) __attribute__((availability(__api_available_platform_##arg5))) __attribute__((availability(__api_available_platform_##arg6))) __attribute__((availability(__api_available_platform_##arg7)))
    
    #define __api_available_get_macro(_0,_1,_2,_3,_4,_5,_6,_7,_8,name,...) name
    
    
    
    
    #define __api_available_begin1(arg0,arg1) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field))))
    
    #define __api_available_begin3(arg0,arg1,arg2,arg3) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field))))
    
    #define __api_available_begin5(arg0,arg1,arg2,arg3,arg4,arg5) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field))))
    
    #define __api_available_begin7(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field))))
    
    #define __api_available_begin_get_macro(_0,_1,_2,_3,_4,_5,_6,_7,_8,name,...) name
    
    
  
    
    #define __api_deprecated_msg1(msg,arg0,arg1) __attribute__((availability(__api_deprecated_platform_##arg0,message=msg))) __attribute__((availability(__api_deprecated_platform_##arg1,message=msg)))
    
    #define __api_deprecated_msg3(msg,arg0,arg1,arg2,arg3) __attribute__((availability(__api_deprecated_platform_##arg0,message=msg))) __attribute__((availability(__api_deprecated_platform_##arg1,message=msg))) __attribute__((availability(__api_deprecated_platform_##arg2,message=msg))) __attribute__((availability(__api_deprecated_platform_##arg3,message=msg)))
    
    #define __api_deprecated_msg5(msg,arg0,arg1,arg2,arg3,arg4,arg5) __attribute__((availability(__api_deprecated_platform_##arg0,message=msg))) __attribute__((availability(__api_deprecated_platform_##arg1,message=msg))) __attribute__((availability(__api_deprecated_platform_##arg2,message=msg))) __attribute__((availability(__api_deprecated_platform_##arg3,message=msg))) __attribute__((availability(__api_deprecated_platform_##arg4,message=msg))) __attribute__((availability(__api_deprecated_platform_##arg5,message=msg)))
    
    #define __api_deprecated_msg7(msg,arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7) __attribute__((availability(__api_deprecated_platform_##arg0,message=msg))) __attribute__((availability(__api_deprecated_platform_##arg1,message=msg))) __attribute__((availability(__api_deprecated_platform_##arg2,message=msg))) __attribute__((availability(__api_deprecated_platform_##arg3,message=msg))) __attribute__((availability(__api_deprecated_platform_##arg4,message=msg))) __attribute__((availability(__api_deprecated_platform_##arg5,message=msg))) __attribute__((availability(__api_deprecated_platform_##arg6,message=msg))) __attribute__((availability(__api_deprecated_platform_##arg7,message=msg)))
    
    #define __api_deprecated_msg_get_macro(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,name,...) name
    
    
    #define __api_deprecated_begin1(msg,arg0,arg1) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field))))
    
    #define __api_deprecated_begin3(msg,arg0,arg1,arg2,arg3) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field))))
    
    #define __api_deprecated_begin5(msg,arg0,arg1,arg2,arg3,arg4,arg5) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field))))
    
    #define __api_deprecated_begin7(msg,arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field))))
    
    #define __api_deprecated_begin_get_macro(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,name,...) name
    
        #define __api_r(rep,x) __attribute__((availability(__api_deprecated_platform_##x,replacement=rep)))
    
    
    #define __api_deprecated_rep1(msg,arg0,arg1) __api_r(msg,arg0) __api_r(msg,arg1)
    
    #define __api_deprecated_rep3(msg,arg0,arg1,arg2,arg3) __api_r(msg,arg0) __api_r(msg,arg1) __api_r(msg,arg2) __api_r(msg,arg3)
    
    #define __api_deprecated_rep5(msg,arg0,arg1,arg2,arg3,arg4,arg5) __api_r(msg,arg0) __api_r(msg,arg1) __api_r(msg,arg2) __api_r(msg,arg3) __api_r(msg,arg4) __api_r(msg,arg5)
    
    #define __api_deprecated_rep7(msg,arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7) __api_r(msg,arg0) __api_r(msg,arg1) __api_r(msg,arg2) __api_r(msg,arg3) __api_r(msg,arg4) __api_r(msg,arg5) __api_r(msg,arg6) __api_r(msg,arg7)
    
    #define __api_deprecated_rep_get_macro(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,name,...) name
    
        #define __api_r_begin(rep,x) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field))))    
    
    
    #define __api_deprecated_begin_rep1(msg,arg0,arg1) __api_r_begin(msg,arg0) __api_r_begin(msg,arg1)
    
    #define __api_deprecated_begin_rep3(msg,arg0,arg1,arg2,arg3) __api_r_begin(msg,arg0) __api_r_begin(msg,arg1) __api_r_begin(msg,arg2) __api_r_begin(msg,arg3)
    
    #define __api_deprecated_begin_rep5(msg,arg0,arg1,arg2,arg3,arg4,arg5) __api_r_begin(msg,arg0) __api_r_begin(msg,arg1) __api_r_begin(msg,arg2) __api_r_begin(msg,arg3) __api_r_begin(msg,arg4) __api_r_begin(msg,arg5)
    
    #define __api_deprecated_begin_rep7(msg,arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7) __api_r_begin(msg,arg0) __api_r_begin(msg,arg1) __api_r_begin(msg,arg2) __api_r_begin(msg,arg3) __api_r_begin(msg,arg4) __api_r_begin(msg,arg5) __api_r_begin(msg,arg6) __api_r_begin(msg,arg7)
    
    #define __api_deprecated_begin_rep_get_macro(_0,_1,_2,_3,_4,_5,_6,_7,_8,_9,name,...) name
    
       
    
    
    #define __api_unavailable1(arg0,arg1) __attribute__((availability(__api_unavailable_platform_##arg0))) __attribute__((availability(__api_unavailable_platform_##arg1)))
    
    #define __api_unavailable3(arg0,arg1,arg2,arg3) __attribute__((availability(__api_unavailable_platform_##arg0))) __attribute__((availability(__api_unavailable_platform_##arg1))) __attribute__((availability(__api_unavailable_platform_##arg2))) __attribute__((availability(__api_unavailable_platform_##arg3)))
    
    #define __api_unavailable5(arg0,arg1,arg2,arg3,arg4,arg5) __attribute__((availability(__api_unavailable_platform_##arg0))) __attribute__((availability(__api_unavailable_platform_##arg1))) __attribute__((availability(__api_unavailable_platform_##arg2))) __attribute__((availability(__api_unavailable_platform_##arg3))) __attribute__((availability(__api_unavailable_platform_##arg4))) __attribute__((availability(__api_unavailable_platform_##arg5)))
    
    #define __api_unavailable7(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7) __attribute__((availability(__api_unavailable_platform_##arg0))) __attribute__((availability(__api_unavailable_platform_##arg1))) __attribute__((availability(__api_unavailable_platform_##arg2))) __attribute__((availability(__api_unavailable_platform_##arg3))) __attribute__((availability(__api_unavailable_platform_##arg4))) __attribute__((availability(__api_unavailable_platform_##arg5))) __attribute__((availability(__api_unavailable_platform_##arg6))) __attribute__((availability(__api_unavailable_platform_##arg7)))
    
    #define __api_unavailable_get_macro(_0,_1,_2,_3,_4,_5,_6,_7,_8,name,...) name
    
    
    #define __api_unavailable_begin1(arg0,arg1) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field))))
    
    #define __api_unavailable_begin3(arg0,arg1,arg2,arg3) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field))))
    
    #define __api_unavailable_begin5(arg0,arg1,arg2,arg3,arg4,arg5) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field))))
    
    #define __api_unavailable_begin7(arg0,arg1,arg2,arg3,arg4,arg5,arg6,arg7) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field)))) )), apply_to = any(record, enum, enum_constant, function, objc_method, objc_category, objc_protocol, objc_interface, objc_property, type_alias, variable, field))))
    
    #define __api_unavailable_begin_get_macro(_0,_1,_2,_3,_4,_5,_6,_7,_8,name,...) name
 
   
 
    #define __swift_compiler_version_at_least(...) 1
   
 
  #define __spi_available(...)
  #define __spi_available_begin(...)
  #define __spi_available_end(...)
    #define __osx_available_starting(_osx, _ios) __availability_internal##_ios
    
    #define __osx_available_but_deprecated_msg(_osxintro, _osxdep, _iosintro, _iosdep, _msg)                                                      __availability_internal##_iosintro##_dep##_iosdep##_msg(_msg)
  #if 1
    
    #define __os_availability_msg(_target, _availability, _msg)  __attribute__((availability(_target,_availability,message=_msg)))
  
    #define __attribute__((availability(_target, _availability)))
    
  #if 1
    
    #define __ios_extension_unavailable(_msg)  
  
    #define 
    
  #if 1
    
    #define __osx_available(_vers)               __attribute__((availability(macosx,introduced=_vers)))
    
  #endif
  #define __osx_available(_vers)
  #if 1
    
    #ifndef __ios_prohibited
      
    #endif
    
    #define __ios_deprecated(_start, _dep, _msg) __attribute__((availability(ios,introduced=_start))) 
  
  #define __ios_deprecated(_start, _dep, _msg)
  #if 1
    
    #ifndef __tvos_prohibited
      
    #endif
    
    #define __tvos_deprecated(_start, _dep, _msg) __attribute__((availability(tvos,introduced=_start))) 
  
  #define __tvos_deprecated(_start, _dep, _msg)
  #if 1
    
    #ifndef __watchos_prohibited
      
    #endif
    
    #define __watchos_deprecated(_start, _dep, _msg) __attribute__((availability(watchos,introduced=_start))) 
  
  #define __watchos_deprecated(_start, _dep, _msg)
  #if 1
    
    #define __swift_unavailable_msg(_msg)         
  
  #define __swift_unavailable_msg(_msg)
   
 #if 1
    
       
    
    
    #define __api_available_end 
#pragma  clang attribute pop
                                                              
    
       
    
    #define __api_deprecated_with_replacement(...) __api_deprecated_rep_get_macro(__va_args__,__api_deprecated_rep8,__api_deprecated_rep7,__api_deprecated_rep6,__api_deprecated_rep5,__api_deprecated_rep4,__api_deprecated_rep3,__api_deprecated_rep2,__api_deprecated_rep1,__api_deprecated_rep0,0,0)(__va_args__)
    
    #define __api_deprecated_end 
#pragma  clang attribute pop
                                                               
    
    #define __api_deprecated_with_replacement_end 
#pragma  clang attribute pop
                                                                                
    
       
    
  
    
    #define __api_unavailable_end 
#pragma  clang attribute pop
                                                                
 
   
  #define __api_available_end(...)
  #define __api_deprecated_end(...)
  #define __api_deprecated_with_replacement(...)
  #define __api_deprecated_with_replacement_end(...)
  #define __api_unavailable_end(...)
   
  #define __spi_available(...)
  #define __spi_available_begin(...)
  #define __spi_available_end(...)
  #define __spi_deprecated(...)
  #define __spi_deprecated_with_replacement(...)
   
   
   
   
   
typedef __signed char           __int8_t;
typedef unsigned char           __uint8_t;
typedef short                   __int16_t;
typedef unsigned short          __uint16_t;
typedef int                     __int32_t;
typedef unsigned int            __uint32_t;
typedef long long               __int64_t;
typedef unsigned long long      __uint64_t;
typedef long                    __darwin_intptr_t;
typedef unsigned int            __darwin_natural_t;
   
typedef int                     __darwin_ct_rune_t;     
   
typedef union {
	char            __mbstate8[128];
	long long       _mbstatel;                      
} __mbstate_t;
typedef __mbstate_t             __darwin_mbstate_t;     
typedef long int        __darwin_ptrdiff_t;     
typedef long unsigned int           __darwin_size_t;        
typedef __builtin_va_list       __darwin_va_list;       
typedef int          __darwin_wchar_t;       
typedef __darwin_wchar_t        __darwin_rune_t;        
typedef int           __darwin_wint_t;        
typedef unsigned long           __darwin_clock_t;       
typedef __uint32_t              __darwin_socklen_t;     
typedef long                    __darwin_ssize_t;       
typedef long                    __darwin_time_t;        
   
typedef __int64_t       __darwin_blkcnt_t;      
typedef __int32_t       __darwin_blksize_t;     
typedef __int32_t       __darwin_dev_t;         
typedef unsigned int    __darwin_fsblkcnt_t;    
typedef unsigned int    __darwin_fsfilcnt_t;    
typedef __uint32_t      __darwin_gid_t;         
typedef __uint32_t      __darwin_id_t;          
typedef __uint64_t      __darwin_ino64_t;       
typedef __darwin_ino64_t __darwin_ino_t;        
typedef __darwin_natural_t __darwin_mach_port_name_t; 
typedef __darwin_mach_port_name_t __darwin_mach_port_t; 
typedef __uint16_t      __darwin_mode_t;        
typedef __int64_t       __darwin_off_t;         
typedef __int32_t       __darwin_pid_t;         
typedef __uint32_t      __darwin_sigset_t;      
typedef __int32_t       __darwin_suseconds_t;   
typedef __uint32_t      __darwin_uid_t;         
typedef __uint32_t      __darwin_useconds_t;    
typedef unsigned char   __darwin_uuid_t[16];
typedef char    __darwin_uuid_string_t[37];
   
// pthread opaque structures
struct __darwin_pthread_handler_rec {
	void (*__routine)(void *);	// routine to call
	void *__arg;			// argument to pass
	struct __darwin_pthread_handler_rec *__next;
};
struct _opaque_pthread_attr_t {
	long __sig;
	char __opaque[56];
};
struct _opaque_pthread_cond_t {
	long __sig;
	char __opaque[40];
};
struct _opaque_pthread_condattr_t {
	long __sig;
	char __opaque[8];
};
struct _opaque_pthread_mutex_t {
	long __sig;
	char __opaque[56];
};
struct _opaque_pthread_mutexattr_t {
	long __sig;
	char __opaque[8];
};
struct _opaque_pthread_once_t {
	long __sig;
	char __opaque[8];
};
struct _opaque_pthread_rwlock_t {
	long __sig;
	char __opaque[192];
};
struct _opaque_pthread_rwlockattr_t {
	long __sig;
	char __opaque[16];
};
struct _opaque_pthread_t {
	long __sig;
	struct __darwin_pthread_handler_rec  *__cleanup_stack;
	char __opaque[8176];
};
typedef struct _opaque_pthread_attr_t __darwin_pthread_attr_t;
typedef struct _opaque_pthread_cond_t __darwin_pthread_cond_t;
typedef struct _opaque_pthread_condattr_t __darwin_pthread_condattr_t;
typedef unsigned long __darwin_pthread_key_t;
typedef struct _opaque_pthread_mutex_t __darwin_pthread_mutex_t;
typedef struct _opaque_pthread_mutexattr_t __darwin_pthread_mutexattr_t;
typedef struct _opaque_pthread_once_t __darwin_pthread_once_t;
typedef struct _opaque_pthread_rwlock_t __darwin_pthread_rwlock_t;
typedef struct _opaque_pthread_rwlockattr_t __darwin_pthread_rwlockattr_t;
typedef struct _opaque_pthread_t *__darwin_pthread_t;
typedef	int		__darwin_nl_item;
typedef	int		__darwin_wctrans_t;
typedef	__uint32_t	__darwin_wctype_t;
                                           
   
   
   
   
   
   
   
   
typedef __signed char           int8_t;
   
typedef short                   int16_t;
   
typedef int                     int32_t;
   
typedef long long               int64_t;
   
typedef unsigned char           u_int8_t;
   
typedef unsigned short                  u_int16_t;
   
typedef unsigned int            u_int32_t;
   
typedef unsigned long long      u_int64_t;
typedef int64_t                 register_t;
   
   
   
typedef __darwin_intptr_t       intptr_t;
   
typedef unsigned long           uintptr_t;
typedef u_int64_t               user_addr_t;
typedef u_int64_t               user_size_t;
typedef int64_t                 user_ssize_t;
typedef int64_t                 user_long_t;
typedef u_int64_t               user_ulong_t;
typedef int64_t                 user_time_t;
typedef int64_t                 user_off_t;
typedef u_int64_t               syscall_arg_t;
typedef __darwin_va_list va_list;
   
typedef __darwin_size_t        size_t;
   
   
   
 
    #define __osx_available_starting(_osx, _ios)
    
    #define __osx_available_but_deprecated_msg(_osxintro, _osxdep, _iosintro, _iosdep, _msg)
  #if 1
    
    #define   __attribute__((availability(_target,_availability,message=_msg)))
  
    #define __attribute__((availability(_target, _availability)))
    
  #if 1
    
    #define   
  
    #define 
    
  #if 1
    
    #define __osx_available(_vers)               __attribute__((availability(macosx,introduced=_vers)))
    
  #endif
  #define __osx_available(_vers)
  #if 1
    
    #ifndef __attribute__((availability(ios,unavailable)))
      
    #endif
    
    #define __ios_deprecated(_start, _dep, _msg) __attribute__((availability(ios,introduced=_start))) 
  
  #define __ios_deprecated(_start, _dep, _msg)
  #if 1
    
    #ifndef __attribute__((availability(tvos,unavailable)))
      
    #endif
    
    #define __tvos_deprecated(_start, _dep, _msg) __attribute__((availability(tvos,introduced=_start))) 
  
  #define __tvos_deprecated(_start, _dep, _msg)
  #if 1
    
    #ifndef __attribute__((availability(watchos,unavailable)))
      
    #endif
    
    #define __watchos_deprecated(_start, _dep, _msg) __attribute__((availability(watchos,introduced=_start))) 
  
  #define __watchos_deprecated(_start, _dep, _msg)
  #if 1
    
    #define __swift_unavailable_msg(_msg)         
  
  #define __swift_unavailable_msg(_msg)
   
 #if 1
    
       
    
    
    #define __api_available_end 
#pragma  clang attribute pop
                                                              
    
       
    
    #define __api_deprecated_with_replacement(...) __api_deprecated_rep_get_macro(__va_args__,__api_deprecated_rep8,__api_deprecated_rep7,__api_deprecated_rep6,__api_deprecated_rep5,__api_deprecated_rep4,__api_deprecated_rep3,__api_deprecated_rep2,__api_deprecated_rep1,__api_deprecated_rep0,0,0)(__va_args__)
    
    #define __api_deprecated_end 
#pragma  clang attribute pop
                                                               
    
    #define __api_deprecated_with_replacement_end 
#pragma  clang attribute pop
                                                                                
    
       
    
  
    
    #define __api_unavailable_end 
#pragma  clang attribute pop
                                                                
 
   
  #define __api_available_end(...)
  #define __api_deprecated_end(...)
  #define __api_deprecated_with_replacement(...)
  #define __api_deprecated_with_replacement_end(...)
  #define __api_unavailable_end(...)
   
  #define __spi_available(...)
  #define __spi_available_begin(...)
  #define __spi_available_end(...)
  #define __spi_deprecated(...)
  #define __spi_deprecated_with_replacement(...)
int     renameat(int, __const char *, int, __const char *) __osx_available_starting(101000, 80000);
int renamex_np(__const char *, __const char *, unsigned int) __osx_available(10.12) __attribute__((availability(ios,introduced=10.0))) __attribute__((availability(tvos,introduced=10.0))) __attribute__((availability(watchos,introduced=3.0)));
int renameatx_np(int, __const char *, int, __const char *, unsigned int) __osx_available(10.12) __attribute__((availability(ios,introduced=10.0))) __attribute__((availability(tvos,introduced=10.0))) __attribute__((availability(watchos,introduced=3.0)));
typedef __darwin_off_t		fpos_t;
   
struct __sbuf {
	unsigned char	*_base;
	int		_size;
};
struct __sfilex;
   
typedef	struct __sfile {
	unsigned char *_p;	
	int	_r;		
	int	_w;		
	short	_flags;		
	short	_file;		
	struct	__sbuf _bf;	
	int	_lbfsize;	
	
	void	*_cookie;	
	int	(* _nullable _close)(void *);
	int	(* _nullable _read) (void *, char *, int);
	fpos_t	(* _nullable _seek) (void *, fpos_t, int);
	int	(* _nullable _write)(void *, __const char *, int);
	
	struct	__sbuf _ub;	
	struct __sfilex *_extra; 
	int	_ur;		
	
	unsigned char _ubuf[3];	
	unsigned char _nbuf[1];	
	
	struct	__sbuf _lb;	
	
	int	_blksize;	
	fpos_t	_offset;	
} file;
   
extern file *__stdinp;
extern file *__stdoutp;
extern file *__stderrp;
	
   
				
void	 clearerr(file *);
int	 fclose(file *);
int	 feof(file *);
int	 ferror(file *);
int	 fflush(file *);
int	 fgetc(file *);
int	 fgetpos(file * restrict, fpos_t *);
char	*fgets(char * restrict, int, file *);
file	*fopen(__const char * restrict __filename, __const char * restrict __mode) __darwin_alias_starting_mac_##1060( __asm("_" "x" ));
int	 fprintf(file * restrict, __const char * restrict, ...) __attribute__((__format__ (__printf__, 2,  3)));
int	 fputc(int, file *);
int	 fputs(__const char * restrict, file * restrict) __asm("_" "x" );
size_t	 fread(void * restrict __ptr, size_t __size, size_t __nitems, file * restrict __stream);
file	*freopen(__const char * restrict, __const char * restrict,
                 file * restrict) __asm("_" "x" );
int	 fscanf(file * restrict, __const char * restrict, ...) __attribute__((__format__ (__scanf__, 2,  3)));
int	 fseek(file *, long, int);
int	 fsetpos(file *, __const fpos_t *);
long	 ftell(file *);
size_t	 fwrite(__const void * restrict __ptr, size_t __size, size_t __nitems, file * restrict __stream) __asm("_" "x" );
int	 getc(file *);
int	 getchar(void);
__deprecated_msg("this function is provided for compatibility reasons only.  due to security concerns inherent in the design of gets(3), it is highly recommended that you use fgets(3) instead.")
char	*gets(char *);
void	 perror(__const char *) __attribute__((__cold__));
int	 printf(__const char * restrict, ...) __attribute__((__format__ (__printf__, 1,  2)));
int	 putc(int, file *);
int	 putchar(int);
int	 puts(__const char *);
int	 remove(__const char *);
int	 rename (__const char *__old, __const char *__new);
void	 rewind(file *);
int	 scanf(__const char * restrict, ...) __attribute__((__format__ (__scanf__, 1,  2)));
void	 setbuf(file * restrict, char * restrict);
int	 setvbuf(file * restrict, char * restrict, int, size_t);
__attribute__((__availability__(swift, unavailable, message="use snprintf instead.")))
__deprecated_msg("this function is provided for compatibility reasons only.  due to security concerns inherent in the design of sprintf(3), it is highly recommended that you use snprintf(3) instead.")
int	 sprintf(char * restrict, __const char * restrict, ...) __attribute__((__format__ (__printf__, 2,  3)));
int	 sscanf(__const char * restrict, __const char * restrict, ...) __attribute__((__format__ (__scanf__, 2,  3)));
file	*tmpfile(void);
__attribute__((__availability__(swift, unavailable, message="use mkstemp(3) instead.")))
__deprecated_msg("this function is provided for compatibility reasons only.  due to security concerns inherent in the design of tmpnam(3), it is highly recommended that you use mkstemp(3) instead.")
char	*tmpnam(char *);
int	 ungetc(int, file *);
int	 vfprintf(file * restrict, __const char * restrict, va_list) __attribute__((__format__ (__printf__, 2,  0)));
int	 vprintf(__const char * restrict, va_list) __attribute__((__format__ (__printf__, 1,  0)));
__attribute__((__availability__(swift, unavailable, message="use vsnprintf instead.")))
__deprecated_msg("this function is provided for compatibility reasons only.  due to security concerns inherent in the design of sprintf(3), it is highly recommended that you use vsnprintf(3) instead.")
int	 vsprintf(char * restrict, __const char * restrict, va_list) __attribute__((__format__ (__printf__, 2,  0)));
   
   
char    *ctermid(char *);
file	*fdopen(int, __const char *) __darwin_alias_starting_mac_##1060( __asm("_" "x" ));
int	 fileno(file *);
   
             
int	 pclose(file *) __attribute__((__availability__(swift, unavailable, message="use posix_spawn apis or nstask instead. (on ios, process spawning is unavailable.)")));
file	*popen(__const char *, __const char *) __darwin_alias_starting_mac_##1060( __asm("_" "x" )) __attribute__((__availability__(swift, unavailable, message="use posix_spawn apis or nstask instead. (on ios, process spawning is unavailable.)")));
           
   
int	__srget(file *);
int	__svfscanf(file *, __const char *, va_list) __attribute__((__format__ (__scanf__, 2,  0)));
int	__swbuf(int, file *);
   
   
void	 flockfile(file *);
int	 ftrylockfile(file *);
void	 funlockfile(file *);
int	 getc_unlocked(file *);
int	 getchar_unlocked(void);
int	 putc_unlocked(int, file *);
int	 putchar_unlocked(int);
int	 getw(file *);
int	 putw(int, file *);
__attribute__((__availability__(swift, unavailable, message="use mkstemp(3) instead.")))
__deprecated_msg("this function is provided for compatibility reasons only.  due to security concerns inherent in the design of tempnam(3), it is highly recommended that you use mkstemp(3) instead.")
char	*tempnam(__const char *__dir, __const char *__prefix) __asm("_" "x" );
   
   
typedef __darwin_off_t          off_t;
int	 fseeko(file * __stream, off_t __offset, int __whence);
off_t	 ftello(file * __stream);
             
int	 snprintf(char * restrict __str, size_t __size, __const char * restrict __format, ...) __attribute__((__format__ (__printf__, 3,  4)));
int	 vfscanf(file * restrict __stream, __const char * restrict __format, va_list) __attribute__((__format__ (__scanf__, 2,  0)));
int	 vscanf(__const char * restrict __format, va_list) __attribute__((__format__ (__scanf__, 1,  0)));
int	 vsnprintf(char * restrict __str, size_t __size, __const char * restrict __format, va_list) __attribute__((__format__ (__printf__, 3,  0)));
int	 vsscanf(__const char * restrict __str, __const char * restrict __format, va_list) __attribute__((__format__ (__scanf__, 2,  0)));
   
   
typedef __darwin_ssize_t        ssize_t;
int	dprintf(int, __const char * restrict, ...) __attribute__((__format__ (__printf__, 2,  3))) __osx_available_starting(1070, 40300);
int	vdprintf(int, __const char * restrict, va_list) __attribute__((__format__ (__printf__, 2,  0))) __osx_available_starting(1070, 40300);
ssize_t getdelim(char ** restrict __linep, size_t * restrict __linecapp, int __delimiter, file * restrict __stream) __osx_available_starting(1070, 40300);
ssize_t getline(char ** restrict __linep, size_t * restrict __linecapp, file * restrict __stream) __osx_available_starting(1070, 40300);
file *fmemopen(void * restrict __buf, size_t __size, __const char * restrict __mode) __api_available_get_macro(macos(10.13), ios(11.0), tvos(11.0), watchos(4.0),__api_available8,__api_available7,__api_available6,__api_available5,__api_available4,__api_available3,__api_available2,__api_available1,__api_available0,0)(macos(10.13), ios(11.0), tvos(11.0), watchos(4.0));
file *open_memstream(char **__bufp, size_t *__sizep) __api_available_get_macro(macos(10.13), ios(11.0), tvos(11.0), watchos(4.0),__api_available8,__api_available7,__api_available6,__api_available5,__api_available4,__api_available3,__api_available2,__api_available1,__api_available0,0)(macos(10.13), ios(11.0), tvos(11.0), watchos(4.0));
             
extern __const int sys_nerr;		
extern __const char *__const sys_errlist[];
int	 asprintf(char ** restrict, __const char * restrict, ...) __attribute__((__format__ (__printf__, 2,  3)));
char	*ctermid_r(char *);
char	*fgetln(file *, size_t *);
__const char *fmtcheck(__const char *, __const char *) __attribute__((format_arg(2)));
int	 fpurge(file *);
void	 setbuffer(file *, char *, int);
int	 setlinebuf(file *);
int	 vasprintf(char ** restrict, __const char * restrict, va_list) __attribute__((__format__ (__printf__, 2,  0)));
   
file	*funopen(__const void *,
                 int (* _nullable)(void *, char *, int),
                 int (* _nullable)(void *, __const char *, int),
                 fpos_t (* _nullable)(void *, fpos_t, int),
                 int (* _nullable)(void *));
   
   
extern int __sprintf_chk (char * restrict, int, size_t,
			  __const char * restrict, ...);
extern int __snprintf_chk (char * restrict, size_t, int, size_t,
			   __const char * restrict, ...);
extern int __vsprintf_chk (char * restrict, int, size_t,
			   __const char * restrict, va_list);
extern int __vsnprintf_chk (char * restrict, size_t, int, size_t,
			    __const char * restrict, va_list);
   
   
   
                                                                                
// aix system headers need stdint.h to be re-enterable while _std_types_t
// is defined until an inclusion of it without _std_types_t occurs, in which
// case the header guard macro is defined.
   
// c99 7.18.3 limits of other integer types
//
//  footnote 219, 220: c++ implementations should define these macros only when
//  __stdc_limit_macros is defined before <stdint.h> is included.
//
//  footnote 222: c++ implementations should define these macros only when
//  __stdc_constant_macros is defined before <stdint.h> is included.
//
// c++11 [cstdint.syn]p2:
//
//  the macros defined by <cstdint> are provided unconditionally. in particular,
//  the symbols __stdc_limit_macros and __stdc_constant_macros (mentioned in
//  footnotes 219, 220, and 222 in the c standard) play no role in c++.
//
// c11 removed the problematic footnotes.
//
// work around this inconsistency by always defining those macros in c++ mode,
// so that a c library implementation which follows the c99 standard can be
// used in c++.
   
   
typedef unsigned char uint8_t;
   
typedef unsigned short uint16_t;
   
typedef unsigned int uint32_t;
   
typedef unsigned long long uint64_t;
typedef int8_t           int_least8_t;
typedef int16_t         int_least16_t;
typedef int32_t         int_least32_t;
typedef int64_t         int_least64_t;
typedef uint8_t         uint_least8_t;
typedef uint16_t       uint_least16_t;
typedef uint32_t       uint_least32_t;
typedef uint64_t       uint_least64_t;
typedef int8_t            int_fast8_t;
typedef int16_t          int_fast16_t;
typedef int32_t          int_fast32_t;
typedef int64_t          int_fast64_t;
typedef uint8_t          uint_fast8_t;
typedef uint16_t        uint_fast16_t;
typedef uint32_t        uint_fast32_t;
typedef uint64_t        uint_fast64_t;
   
typedef long int intmax_t;
   
typedef long unsigned int uintmax_t;
   
   
               
   
 
    #define __osx_available_starting(_osx, _ios)
    
    #define __osx_available_but_deprecated_msg(_osxintro, _osxdep, _iosintro, _iosdep, _msg)
  #if 1
    
    #define   __attribute__((availability(_target,_availability,message=_msg)))
  
    #define __attribute__((availability(_target, _availability)))
    
  #if 1
    
    #define   
  
    #define 
    
  #if 1
    
    #define __osx_available(_vers)               __attribute__((availability(macosx,introduced=_vers)))
    
  #endif
  #define __osx_available(_vers)
  #if 1
    
    #ifndef __attribute__((availability(ios,unavailable)))
      
    #endif
    
    #define __ios_deprecated(_start, _dep, _msg) __attribute__((availability(ios,introduced=_start))) 
  
  #define __ios_deprecated(_start, _dep, _msg)
  #if 1
    
    #ifndef __attribute__((availability(tvos,unavailable)))
      
    #endif
    
    #define __tvos_deprecated(_start, _dep, _msg) __attribute__((availability(tvos,introduced=_start))) 
  
  #define __tvos_deprecated(_start, _dep, _msg)
  #if 1
    
    #ifndef __attribute__((availability(watchos,unavailable)))
      
    #endif
    
    #define __watchos_deprecated(_start, _dep, _msg) __attribute__((availability(watchos,introduced=_start))) 
  
  #define __watchos_deprecated(_start, _dep, _msg)
  #if 1
    
    #define __swift_unavailable_msg(_msg)         
  
  #define __swift_unavailable_msg(_msg)
   
 #if 1
    
       
    
    
    #define __api_available_end 
#pragma  clang attribute pop
                                                              
    
       
    
    #define __api_deprecated_with_replacement(...) __api_deprecated_rep_get_macro(__va_args__,__api_deprecated_rep8,__api_deprecated_rep7,__api_deprecated_rep6,__api_deprecated_rep5,__api_deprecated_rep4,__api_deprecated_rep3,__api_deprecated_rep2,__api_deprecated_rep1,__api_deprecated_rep0,0,0)(__va_args__)
    
    #define __api_deprecated_end 
#pragma  clang attribute pop
                                                               
    
    #define __api_deprecated_with_replacement_end 
#pragma  clang attribute pop
                                                                                
    
       
    
  
    
    #define __api_unavailable_end 
#pragma  clang attribute pop
                                                                
 
   
  #define __api_available_end(...)
  #define __api_deprecated_end(...)
  #define __api_deprecated_with_replacement(...)
  #define __api_deprecated_with_replacement_end(...)
  #define __api_unavailable_end(...)
   
  #define __spi_available(...)
  #define __spi_available_begin(...)
  #define __spi_available_end(...)
  #define __spi_deprecated(...)
  #define __spi_deprecated_with_replacement(...)
   
   
struct timeval
{
	__darwin_time_t         tv_sec;         
	__darwin_suseconds_t    tv_usec;        
};
   
typedef __darwin_id_t   id_t;           
   
typedef __uint64_t      rlim_t;
   
   
   
   
   
   
   
   
struct  rusage {
	struct timeval ru_utime;        
	struct timeval ru_stime;        
	
	
 long    ru_maxrss;              
	long    ru_ixrss;               
	long    ru_idrss;               
	long    ru_isrss;               
	long    ru_minflt;              
	long    ru_majflt;              
	long    ru_nswap;               
	long    ru_inblock;             
	long    ru_oublock;             
	long    ru_msgsnd;              
	long    ru_msgrcv;              
	long    ru_nsignals;            
	long    ru_nvcsw;               
	long    ru_nivcsw;              
};
   
   
typedef void *rusage_info_t;
struct rusage_info_v0 {
	uint8_t  ri_uuid[16];
	uint64_t ri_user_time;
	uint64_t ri_system_time;
	uint64_t ri_pkg_idle_wkups;
	uint64_t ri_interrupt_wkups;
	uint64_t ri_pageins;
	uint64_t ri_wired_size;
	uint64_t ri_resident_size;
	uint64_t ri_phys_footprint;
	uint64_t ri_proc_start_abstime;
	uint64_t ri_proc_exit_abstime;
};
struct rusage_info_v1 {
	uint8_t  ri_uuid[16];
	uint64_t ri_user_time;
	uint64_t ri_system_time;
	uint64_t ri_pkg_idle_wkups;
	uint64_t ri_interrupt_wkups;
	uint64_t ri_pageins;
	uint64_t ri_wired_size;
	uint64_t ri_resident_size;
	uint64_t ri_phys_footprint;
	uint64_t ri_proc_start_abstime;
	uint64_t ri_proc_exit_abstime;
	uint64_t ri_child_user_time;
	uint64_t ri_child_system_time;
	uint64_t ri_child_pkg_idle_wkups;
	uint64_t ri_child_interrupt_wkups;
	uint64_t ri_child_pageins;
	uint64_t ri_child_elapsed_abstime;
};
struct rusage_info_v2 {
	uint8_t  ri_uuid[16];
	uint64_t ri_user_time;
	uint64_t ri_system_time;
	uint64_t ri_pkg_idle_wkups;
	uint64_t ri_interrupt_wkups;
	uint64_t ri_pageins;
	uint64_t ri_wired_size;
	uint64_t ri_resident_size;
	uint64_t ri_phys_footprint;
	uint64_t ri_proc_start_abstime;
	uint64_t ri_proc_exit_abstime;
	uint64_t ri_child_user_time;
	uint64_t ri_child_system_time;
	uint64_t ri_child_pkg_idle_wkups;
	uint64_t ri_child_interrupt_wkups;
	uint64_t ri_child_pageins;
	uint64_t ri_child_elapsed_abstime;
	uint64_t ri_diskio_bytesread;
	uint64_t ri_diskio_byteswritten;
};
struct rusage_info_v3 {
	uint8_t  ri_uuid[16];
	uint64_t ri_user_time;
	uint64_t ri_system_time;
	uint64_t ri_pkg_idle_wkups;
	uint64_t ri_interrupt_wkups;
	uint64_t ri_pageins;
	uint64_t ri_wired_size;
	uint64_t ri_resident_size;
	uint64_t ri_phys_footprint;
	uint64_t ri_proc_start_abstime;
	uint64_t ri_proc_exit_abstime;
	uint64_t ri_child_user_time;
	uint64_t ri_child_system_time;
	uint64_t ri_child_pkg_idle_wkups;
	uint64_t ri_child_interrupt_wkups;
	uint64_t ri_child_pageins;
	uint64_t ri_child_elapsed_abstime;
	uint64_t ri_diskio_bytesread;
	uint64_t ri_diskio_byteswritten;
	uint64_t ri_cpu_time_qos_default;
	uint64_t ri_cpu_time_qos_maintenance;
	uint64_t ri_cpu_time_qos_background;
	uint64_t ri_cpu_time_qos_utility;
	uint64_t ri_cpu_time_qos_legacy;
	uint64_t ri_cpu_time_qos_user_initiated;
	uint64_t ri_cpu_time_qos_user_interactive;
	uint64_t ri_billed_system_time;
	uint64_t ri_serviced_system_time;
};
struct rusage_info_v4 {
	uint8_t  ri_uuid[16];
	uint64_t ri_user_time;
	uint64_t ri_system_time;
	uint64_t ri_pkg_idle_wkups;
	uint64_t ri_interrupt_wkups;
	uint64_t ri_pageins;
	uint64_t ri_wired_size;
	uint64_t ri_resident_size;
	uint64_t ri_phys_footprint;
	uint64_t ri_proc_start_abstime;
	uint64_t ri_proc_exit_abstime;
	uint64_t ri_child_user_time;
	uint64_t ri_child_system_time;
	uint64_t ri_child_pkg_idle_wkups;
	uint64_t ri_child_interrupt_wkups;
	uint64_t ri_child_pageins;
	uint64_t ri_child_elapsed_abstime;
	uint64_t ri_diskio_bytesread;
	uint64_t ri_diskio_byteswritten;
	uint64_t ri_cpu_time_qos_default;
	uint64_t ri_cpu_time_qos_maintenance;
	uint64_t ri_cpu_time_qos_background;
	uint64_t ri_cpu_time_qos_utility;
	uint64_t ri_cpu_time_qos_legacy;
	uint64_t ri_cpu_time_qos_user_initiated;
	uint64_t ri_cpu_time_qos_user_interactive;
	uint64_t ri_billed_system_time;
	uint64_t ri_serviced_system_time;
	uint64_t ri_logical_writes;
	uint64_t ri_lifetime_max_phys_footprint;
	uint64_t ri_instructions;
	uint64_t ri_cycles;
	uint64_t ri_billed_energy;
	uint64_t ri_serviced_energy;
	uint64_t ri_interval_max_phys_footprint;
	uint64_t ri_runnable_time;
};
struct rusage_info_v5 {
	uint8_t  ri_uuid[16];
	uint64_t ri_user_time;
	uint64_t ri_system_time;
	uint64_t ri_pkg_idle_wkups;
	uint64_t ri_interrupt_wkups;
	uint64_t ri_pageins;
	uint64_t ri_wired_size;
	uint64_t ri_resident_size;
	uint64_t ri_phys_footprint;
	uint64_t ri_proc_start_abstime;
	uint64_t ri_proc_exit_abstime;
	uint64_t ri_child_user_time;
	uint64_t ri_child_system_time;
	uint64_t ri_child_pkg_idle_wkups;
	uint64_t ri_child_interrupt_wkups;
	uint64_t ri_child_pageins;
	uint64_t ri_child_elapsed_abstime;
	uint64_t ri_diskio_bytesread;
	uint64_t ri_diskio_byteswritten;
	uint64_t ri_cpu_time_qos_default;
	uint64_t ri_cpu_time_qos_maintenance;
	uint64_t ri_cpu_time_qos_background;
	uint64_t ri_cpu_time_qos_utility;
	uint64_t ri_cpu_time_qos_legacy;
	uint64_t ri_cpu_time_qos_user_initiated;
	uint64_t ri_cpu_time_qos_user_interactive;
	uint64_t ri_billed_system_time;
	uint64_t ri_serviced_system_time;
	uint64_t ri_logical_writes;
	uint64_t ri_lifetime_max_phys_footprint;
	uint64_t ri_instructions;
	uint64_t ri_cycles;
	uint64_t ri_billed_energy;
	uint64_t ri_serviced_energy;
	uint64_t ri_interval_max_phys_footprint;
	uint64_t ri_runnable_time;
	uint64_t ri_flags;
};
struct rusage_info_v6 {
	uint8_t  ri_uuid[16];
	uint64_t ri_user_time;
	uint64_t ri_system_time;
	uint64_t ri_pkg_idle_wkups;
	uint64_t ri_interrupt_wkups;
	uint64_t ri_pageins;
	uint64_t ri_wired_size;
	uint64_t ri_resident_size;
	uint64_t ri_phys_footprint;
	uint64_t ri_proc_start_abstime;
	uint64_t ri_proc_exit_abstime;
	uint64_t ri_child_user_time;
	uint64_t ri_child_system_time;
	uint64_t ri_child_pkg_idle_wkups;
	uint64_t ri_child_interrupt_wkups;
	uint64_t ri_child_pageins;
	uint64_t ri_child_elapsed_abstime;
	uint64_t ri_diskio_bytesread;
	uint64_t ri_diskio_byteswritten;
	uint64_t ri_cpu_time_qos_default;
	uint64_t ri_cpu_time_qos_maintenance;
	uint64_t ri_cpu_time_qos_background;
	uint64_t ri_cpu_time_qos_utility;
	uint64_t ri_cpu_time_qos_legacy;
	uint64_t ri_cpu_time_qos_user_initiated;
	uint64_t ri_cpu_time_qos_user_interactive;
	uint64_t ri_billed_system_time;
	uint64_t ri_serviced_system_time;
	uint64_t ri_logical_writes;
	uint64_t ri_lifetime_max_phys_footprint;
	uint64_t ri_instructions;
	uint64_t ri_cycles;
	uint64_t ri_billed_energy;
	uint64_t ri_serviced_energy;
	uint64_t ri_interval_max_phys_footprint;
	uint64_t ri_runnable_time;
	uint64_t ri_flags;
	uint64_t ri_user_ptime;
	uint64_t ri_system_ptime;
	uint64_t ri_pinstructions;
	uint64_t ri_pcycles;
	uint64_t ri_energy_nj;
	uint64_t ri_penergy_nj;
	uint64_t ri_secure_time_in_system;
	uint64_t ri_secure_ptime_in_system;
	uint64_t ri_reserved[12];
};
typedef struct rusage_info_v6 rusage_info_current;
   
   
   
   
struct rlimit {
	rlim_t  rlim_cur;               
	rlim_t  rlim_max;               
};
   
   
   
   
struct proc_rlimit_control_wakeupmon {
	uint32_t wm_flags;
	int32_t wm_rate;
};
int     getpriority(int, id_t);
int     getiopolicy_np(int, int) __osx_available_starting(1050, 20000);
int     getrlimit(int, struct rlimit *) __asm("_" "x" );
int     getrusage(int, struct rusage *);
int     setpriority(int, id_t, int);
int     setiopolicy_np(int, int, int) __osx_available_starting(1050, 20000);
int     setrlimit(int, __const struct rlimit *) __asm("_" "x" );
double get_memory_(void)
{
  long kbytes;
  double mbytes;
  struct rusage ru;
  getrusage(0, &ru);
 
  kbytes = ru.ru_maxrss;
  mbytes = ((double) kbytes) / 1024.0;
  return mbytes;
}
