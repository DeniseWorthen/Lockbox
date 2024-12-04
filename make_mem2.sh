#!/bin/bash

set -x

grep  'Exiting  WW3 Run :  - MemInfo:' PET0208.ESMF_LogFile |grep VmPeak| awk '{print $13}'>vmpeak.ww3.0208.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0308.ESMF_LogFile |grep VmPeak| awk '{print $13}'>vmpeak.ww3.0308.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0408.ESMF_LogFile |grep VmPeak| awk '{print $13}'>vmpeak.ww3.0408.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0508.ESMF_LogFile |grep VmPeak| awk '{print $13}'>vmpeak.ww3.0508.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0608.ESMF_LogFile |grep VmPeak| awk '{print $13}'>vmpeak.ww3.0608.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0708.ESMF_LogFile |grep VmPeak| awk '{print $13}'>vmpeak.ww3.0708.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0808.ESMF_LogFile |grep VmPeak| awk '{print $13}'>vmpeak.ww3.0808.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0908.ESMF_LogFile |grep VmPeak| awk '{print $13}'>vmpeak.ww3.0908.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET1008.ESMF_LogFile |grep VmPeak| awk '{print $13}'>vmpeak.ww3.1008.dat

grep  'Exiting  WW3 Run :  - MemInfo:' PET0258.ESMF_LogFile |grep VmPeak:| awk '{print $13}'>vmpeak.ww3.0258.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0358.ESMF_LogFile |grep VmPeak:| awk '{print $13}'>vmpeak.ww3.0358.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0458.ESMF_LogFile |grep VmPeak:| awk '{print $13}'>vmpeak.ww3.0458.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0558.ESMF_LogFile |grep VmPeak:| awk '{print $13}'>vmpeak.ww3.0558.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0658.ESMF_LogFile |grep VmPeak:| awk '{print $13}'>vmpeak.ww3.0658.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0758.ESMF_LogFile |grep VmPeak:| awk '{print $13}'>vmpeak.ww3.0758.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0858.ESMF_LogFile |grep VmPeak:| awk '{print $13}'>vmpeak.ww3.0858.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0958.ESMF_LogFile |grep VmPeak:| awk '{print $13}'>vmpeak.ww3.0958.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET1023.ESMF_LogFile |grep VmPeak:| awk '{print $13}'>vmpeak.ww3.1023.dat

#-------------

grep  'Exiting  WW3 Run :  - MemInfo:' PET0208.ESMF_LogFile |grep VmRSS:| awk '{print $13}'>vmrss.ww3.0208.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0308.ESMF_LogFile |grep VmRSS:| awk '{print $13}'>vmrss.ww3.0308.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0408.ESMF_LogFile |grep VmRSS:| awk '{print $13}'>vmrss.ww3.0408.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0508.ESMF_LogFile |grep VmRSS:| awk '{print $13}'>vmrss.ww3.0508.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0608.ESMF_LogFile |grep VmRSS:| awk '{print $13}'>vmrss.ww3.0608.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0708.ESMF_LogFile |grep VmRSS:| awk '{print $13}'>vmrss.ww3.0708.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0808.ESMF_LogFile |grep VmRSS:| awk '{print $13}'>vmrss.ww3.0808.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0908.ESMF_LogFile |grep VmRSS:| awk '{print $13}'>vmrss.ww3.0908.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET1008.ESMF_LogFile |grep VmRSS:| awk '{print $13}'>vmrss.ww3.1008.dat

grep  'Exiting  WW3 Run :  - MemInfo:' PET0258.ESMF_LogFile |grep VmRSS:| awk '{print $13}'>vmrss.ww3.0258.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0358.ESMF_LogFile |grep VmRSS:| awk '{print $13}'>vmrss.ww3.0358.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0458.ESMF_LogFile |grep VmRSS:| awk '{print $13}'>vmrss.ww3.0458.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0558.ESMF_LogFile |grep VmRSS:| awk '{print $13}'>vmrss.ww3.0558.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0658.ESMF_LogFile |grep VmRSS:| awk '{print $13}'>vmrss.ww3.0658.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0758.ESMF_LogFile |grep VmRSS:| awk '{print $13}'>vmrss.ww3.0758.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0858.ESMF_LogFile |grep VmRSS:| awk '{print $13}'>vmrss.ww3.0858.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0958.ESMF_LogFile |grep VmRSS:| awk '{print $13}'>vmrss.ww3.0958.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET1023.ESMF_LogFile |grep VmRSS:| awk '{print $13}'>vmrss.ww3.1023.dat

#-------------

grep  'Exiting  WW3 Run :  - MemInfo:' PET0208.ESMF_LogFile |grep CommittedPercent:| awk '{print $15}'>pctcommt.ww3.0208.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0308.ESMF_LogFile |grep CommittedPercent:| awk '{print $15}'>pctcommt.ww3.0308.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0408.ESMF_LogFile |grep CommittedPercent:| awk '{print $15}'>pctcommt.ww3.0408.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0508.ESMF_LogFile |grep CommittedPercent:| awk '{print $15}'>pctcommt.ww3.0508.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0608.ESMF_LogFile |grep CommittedPercent:| awk '{print $15}'>pctcommt.ww3.0608.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0708.ESMF_LogFile |grep CommittedPercent:| awk '{print $15}'>pctcommt.ww3.0708.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0808.ESMF_LogFile |grep CommittedPercent:| awk '{print $15}'>pctcommt.ww3.0808.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0908.ESMF_LogFile |grep CommittedPercent:| awk '{print $15}'>pctcommt.ww3.0908.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET1008.ESMF_LogFile |grep CommittedPercent:| awk '{print $15}'>pctcommt.ww3.1008.dat

grep  'Exiting  WW3 Run :  - MemInfo:' PET0258.ESMF_LogFile |grep CommittedPercent:| awk '{print $15}'>pctcommt.ww3.0258.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0358.ESMF_LogFile |grep CommittedPercent:| awk '{print $15}'>pctcommt.ww3.0358.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0458.ESMF_LogFile |grep CommittedPercent:| awk '{print $15}'>pctcommt.ww3.0458.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0558.ESMF_LogFile |grep CommittedPercent:| awk '{print $15}'>pctcommt.ww3.0558.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0658.ESMF_LogFile |grep CommittedPercent:| awk '{print $15}'>pctcommt.ww3.0658.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0758.ESMF_LogFile |grep CommittedPercent:| awk '{print $15}'>pctcommt.ww3.0758.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0858.ESMF_LogFile |grep CommittedPercent:| awk '{print $15}'>pctcommt.ww3.0858.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET0958.ESMF_LogFile |grep CommittedPercent:| awk '{print $15}'>pctcommt.ww3.0958.dat
grep  'Exiting  WW3 Run :  - MemInfo:' PET1023.ESMF_LogFile |grep CommittedPercent:| awk '{print $15}'>pctcommt.ww3.1023.dat
