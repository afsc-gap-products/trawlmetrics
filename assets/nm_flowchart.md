``` mermaid
graph TD
HA[\"Height Data<br>(Both Vessels)\"]-->HB[\"Gate Filter<br>0#lt;Height#lt;6\"]
    HB[\"Gate Filter<br>3#lt;Height#lt;10\"]-->HC[\"#gt;50 pings?\"]
    HC[\"#gt;50 pings?\"]-->|Yes| HU[Predictors]
    HC[\"#gt;50 pings?\"]-->|No| HG[Good Performance?]
    HG[Good Performance?]-->|Yes| HJ[Spread and Net Number Available?]
    HG[Good Performance?]-->|No| HT[Accept Height]
    HD[Vessel<br>Net Number<br>Speed<br>Scope Ratio<br>Bottom Depth<br>Catch Weight<br>Good Obs. Spread]-->HU[Predictors]
    subgraph Generalized Additive Models
      HU[Predictors]-.->HO[GAM: Height]
      HU[Predictors]-.->HQ[GAM: Height<br>No NN]
      HU[Predictors]-.->HR[GAM: Height<br>No Spd]
      HU[Predictors]-.->HS[GAM: Height<br>No Spd and NN]
      HJ[Spread and Net Number Available?]-->|Spd: Yes<br>NN: Yes| HO[GAM: Height]
      HJ[Spread and Net Number Available?]-->|Spd: Yes<br>NN: No| HQ[GAM: Height<br>No NN]
      HJ[Spread and Net Number Available?]-->|Spd: No<br>NN: Yes| HR[GAM: Height<br>No Spd]
      HJ[Spread and Net Number Available?]-->|Spd: No<br>NN: No| HS[GAM: Height<br>No Spd and NN]
    end
    HO[GAM: Height]-->HT[Accept Height]
    HQ[GAM: Height<br>No NN]-->HT[Accept Height]
    HR[GAM: Height<br>No Spd]-->HT[Accept Height]
    HS[GAM: Height<br>No Spd and NN]-->HT[Accept Height]
```
