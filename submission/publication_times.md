### Information about the Publication of our data

# Our Quarterly Data 
## quarterly gdp growth
Our low frequency time series (i.e. gdp growth) from kof is sourced from seco

-> 2025-Q2 was released on the 29.08.2025
-> 2025-Q1 was released on the 02.06.2025
-> 2024-Q4 was released on the 26.02.2025

**On avg. the quarterly gdp data is released 2 months after the quarter is done...**
Source: https://www.seco.admin.ch/seco/en/home/wirtschaftslage---wirtschaftspolitik/Wirtschaftslage/bip-quartalsschaetzungen-.html (Archive of press releases)
Q1 -> 1.1 - 31.3
Q2 -> 1.4 - 30.6
Q3 -> 1.7 - 30.9
Q4 -> 1.10 - 31.12

## quarterly inflation data 

acc. to koma metadata: source: BFS, KOF

Assumption: BFS publishes monthly cpi data, kof avgs it. 

BFS releases the inflation data monthly, source: https://www.bfs.admin.ch/bfs/de/home/statistiken/preise/erhebungen/lik.html

Publikationsdaten:

-> 10.2025 -> 3.11.25
-> 09.2025 -> 2.10.25
-> 08.2025 -> 4.09.25

**here shift 2**

**BFS publishes the data a couple days after the month is over -> KOF published quarterly cpi data a couple days after the quarter is over**

Source: https://www.bfs.admin.ch/bfs/de/home/statistiken/katalog.html?embargoFrom=2024-11-18T00%3A00%3A00.000Z&extendedSearch=landesindex&institution=900065



## quarterly exchange rates

acc. to koma metadata: source: SNB, KOF

assumptions: I assume SNB publishes the data, `devkum` -> foreign exchange rates monthly (M0) -> and then KOF avg. to quarterly 

Publication:
 
-> 2025-10 -> released 3.11.25 
-> 2025-09 -> released 1.10.25
-> 2025-08 -> released 1.9.25
-> 2027-07 -> released 4.8.25

**here shift 2**
**SNB released exchange rate data a few days after the month is over -> KOF published quarterly wkfreuro data a couple days after the quarter is over**

Source: https://data.snb.ch/en/publishingSet/A

# Our monthly data

## KOF Baro
Our high frequency time series (for midas) is the KOF Baro, which is released by KOF,

10-2025 -> 30.10
09-2025 -> 30.9

**KOF Baro is released as soon as month is done**

Source: https://kof.ethz.ch/en/news-and-events/media/press-releases.html

## SNB monthly data -> devkum, plkopr, snboffzisa, amarbma, smi

### devkum
see above...
**devkum published a couple days after end of month**

### plkopr

-  2025-10 -> 21.11.2025 
-  2025-09 -> 21.10.2025

**publishing of plkopr 1 month behind on avg.** -> hence, here a cutoff shift of 1 is more suitable!!

Source: https://data.snb.ch/en/publishingSet/B 

### SMI

- smi data synthesised from returns/avg. price from start to end of month, -> so available after last business day of the month (i.e. start of next month)

**available after last business day of the month (i.e. start of next month)** 

### snboffzisa 
- offizieller zinssatz is decided after a quarterly monetary policy assessment, which is then published in a press release -> in March, June, September and December

**here, since the data is released every 3 months, approx. we know in advance what the policy rate looks like until next press release**

Source: https://data.snb.ch/de/publishingSet/A

### amarbma

-  2025-10 -> 21.11.2025 
-  2025-09 -> 21.10.2025

Source: https://data.snb.ch/en/publishingSet/B 

**publishing of plkopr 1 month behind on avg.** -> hence, here a cutoff shift of 1 is more suitable!!

## Fazit!

**quarterly:**

suitable for **permanent** 2m shift:
-> gdp growth ~ baro 
-> gdp growth ~ devkum/baro/smi 

For the dataset pertaining to cpi/smi/devkum


suitable for **adjusting shift** (from 0m,1m to 2m):

-> qrtly inflation 

-> qrtly exchange rate 


**monthly:**

-> available EOM:

- (baro) 
- devkum
- smi

-> available 1 month later:

- plkopr
- amarbma 

-> available 3 months in advance (based on qrtly press release):

- snboffzisa 


**We need to make differentiations when it comes to using data in a shifted way, i.e. depending on the y we are predicting, include x's in different times**