# Spread and Height Processing Flow Charts

The flowcharts below show how spread and height measurements obtained using spread sensors attached at the wing tips and height sensor attached to the center of the headrope.

### GOA and AI Survey Height

``` mermaid
graph TD
    HA["Height Data<br>(Both Vessels)"]-->HB["Gate Filter<br>0&lt;Height&lt;6"]
    HB["Gate Filter<br>3&lt;Height&lt;10"]-->HC["&gt;50 pings?"]
    HC["&gt;50 pings?"]-->|Yes| HU[Predictors]
    HC["&gt;50 pings?"]-->|No| HG[Good Performance?]
    HG[Good Performance?]-->|Yes| HJ[Spread and Net Number Available?]
    HG[Good Performance?]-->|No| HT[Accepted Height]
    HD[Vessel<br>Net Number<br>Speed<br>Scope Ratio<br>Bottom Depth<br>Catch Weight<br>Good Obs. Spread]-->HU[Predictors]
    subgraph Generalized Additive Models
      HU[Predictors]-.->HO[GAM: Height]
      HU[Predictors]-.->HQ[GAM: Height<br>No NN]
      HU[Predictors]-.->HR[GAM: Height<br>No Spd]
      HU[Predictors]-.->HS[GAM: Height<br>No Spd or NN]
      HJ[Spread and Net Number Available?]-->|Spd: Yes<br>NN: Yes| HO[GAM: Height]
      HJ[Spread and Net Number Available?]-->|Spd: Yes<br>NN: No| HQ[GAM: Height<br>No NN]
      HJ[Spread and Net Number Available?]-->|Spd: No<br>NN: Yes| HR[GAM: Height<br>No Spd]
      HJ[Spread and Net Number Available?]-->|Spd: No<br>NN: No| HS[GAM: Height<br>No Spd or NN]
    end
    HO[GAM: Height]-->HT[Accepted Height]
    HQ[GAM: Height<br>No NN]-->HT[Accepted Height]
    HR[GAM: Height<br>No Spd]-->HT[Accepted Height]
    HS[GAM: Height<br>No Spd and NN]-->HT[Accepted Height]
```

### GOA and AI Survey Height GAMs
| Model | Formula |
|------------|------------|
| Height | net_height ~ factor(vessel) + factor(net_number) + s(net_spread) + s(depth) + s(speed) + s(scope_ratio) + s(total_catch_weight) |
| Height No Spread | net_height ~ factor(vessel) + factor(net_number) + s(depth) + s(speed) + s(scope_ratio) + s(total_catch_weight) |
| Height No Net Number | net_height ~ factor(vessel) + s(net_spread) + s(depth) + s(speed) + s(scope_ratio) + s(total_catch_weight) |
| Height No Spread or Net Number | net_height ~ factor(vessel) + s(depth) + s(speed) + s(scope_ratio) + s(total_catch_weight) |


### GOA and AI Survey Spread

This data processing method applies sequential outlier rejection (SOR) to spread data and estimates spread for hauls without sufficient spread using generalized additive models. Height is estimated prior to estimating missing spread.

``` mermaid
graph TD
    SA["Spread Data<br>(Both Vessels)"]-->SB["Gate Filter<br>10&lt;Spread&lt;22"]
    SB["Gate Filter<br>10&lt;Spread&lt;22"]-->SC["&gt;50 pings?"]
    SC["&gt;50 pings?"]-->|Yes| SV[Sequential Outlier<br>Rejection]
    SC["&gt;50 pings?"]-->|No| SF["&gt;0 pings"]
    SV[Sequential Outlier<br>Rejection]-->SW[Inspect Plot]
    SF["&gt;0 pings?"]-->|No| SL[Height and Net Number Available?]
    SF["&gt;0 pings?"]-->|Yes| SI[Inspect plot:<br>Spread OK?]
    SI[Inspect plot:<br>Spread OK?]-->|No| SL[Height and Net Number Available?]
    SI[Inspect plot:<br>Spread OK?]-->|Yes| SU[Accepted Spread]
    SV[Sequential Outlier<br>Rejection]-->SX[Predictors]
    SD[Vessel<br>Net Number<br>Speed<br>Scope Ratio<br>Bottom Depth<br>Catch Weight<br>Good Obs. Height*]-->SX[Predictors]
    subgraph Generalized Additive Models
      SL[Height and Net Number Available?]-->|Ht: Yes<br>NN: Yes| SQ[GAM: Spread]
      SL[Height and Net Number Available?]-->|Ht: Yes<br>NN: No| SR[GAM: Spread<br>No NN]
      SL[Height and Net Number Available?]-->|Ht: No<br>NN: Yes| SS[GAM: Spread<br>No Ht]
      SL[Height and Net Number Available?]-->|Ht: No<br>NN: No| ST[GAM: Spread<br>No Ht or NN]
      SX[Predictors]-.->SQ[GAM: Spread]
      SX[Predictors]-.->SR[GAM: Spread<br>No NN]
      SX[Predictors]-.->SS[GAM: Spread<br>No Ht]
      SX[Predictors]-.->ST[GAM: Spread<br>No Ht or NN]
    end
    SQ[GAM: Spread]-->SU[Accepted Spread]
    SR[GAM: Spread<br>No NN]-->SU[Accepted Spread]
    SS[GAM: Spread<br>No Ht]-->SU[Accepted Spread]
    ST[GAM: Spread<br>No Ht and NN]-->SU[Accepted Spread]
    SW[Inspect Plot]-->SU[Accepted Spread]
```
*Good obs height means height pings have been gated and there are n > 50.

### GOA and AI Survey Spread GAMs
| Model | Formula |
|------------|------------|
| Spread | net_spread ~ factor(vessel) + factor(net_number) + s(net_height) + s(depth) + s(speed) + s(scope_ratio) + s(total_catch_weight) |
| Spread No Spread | net_spread ~ factor(vessel) + factor(net_number) + s(depth) + s(speed) + s(scope_ratio) + s(total_catch_weight) |
| Spread No Net Number | net_spread ~ factor(vessel) + s(net_height) + s(depth) + s(speed) + s(scope_ratio) + s(total_catch_weight) |
| Spread No Spread or Net Number | net_spread ~ factor(vessel) + s(depth) + s(speed) + s(scope_ratio) + s(total_catch_weight) |


### EBS Survey Height
``` mermaid
graph TD
    HA["Height Data<br>(One Vessel)"]-->HB["Gate Filter<br>0&lt;Height&lt;6"]
    HB["Gate Filter<br>3&lt;Height&lt;10"]-->HC["&gt;150 pings?"]
    HC["&gt;150 pings?"]-->|No| HG[Mean height for scope]
    HC["&gt;150 pings?"]-->|Yes| HH[Accepted Height]
    HG[Mean height for scope]-->HH[Accepted Height]
```

### EBS Survey Spread

This data processing method applies sequential outlier rejection (SOR) to spread data and estimates spread for hauls without sufficient spread using a generalized linear model. Height is estimated prior to estimating missing spread. A correction is applied to convert Marport spread values to the value that would have been estimated from Netmind.

``` mermaid
graph TD
    SA["Spread Data<br>(One Vessel)"]-->SB["Gate Filter<br>10&lt;Spread&lt;22"]
    SB["Gate Filter<br>10&lt;Spread&lt;22"]-->SC["&gt;50 pings?"]
    SC["&gt;50 pings?"]-->|No| SF["&gt;0 pings?"]
    SC["&gt;50 pings?"]-->|Yes| SN[Sequential Outlier<br>Rejection]
    SD[Inverse Scope<br><b>Good Obs. Height^</b>]-->SP[Predictors]
    SN[Sequential Outlier<br>Rejection]-->SO[Inspect Plot]
    SO[Inspect Plot]-->SP[Predictors]
    SO[Inspect Plot]-->SQ[Marport to Netmind<br>Conversion]
    SF["&gt;0 pings?"]-->|No| SL[Spread GLM]
    SF["&gt;0 pings?"]-->|Yes| SI[Inspect plot:<br>Spread OK?]
    SP[Predictors]-.->SL[Spread GLM]
    SI[Inspect plot:<br>Spread OK?]-->|No| SL[Spread GLM]
    SI[Inspect plot:<br>Spread OK?]-->|Yes| SQ[Marport to Netmind<br>Conversion]
    SL[Spread GLM]-->SQ[Marport to Netmind<br>Conversion]  
    SQ[Marport to Netmind<br>Conversion]-->SM[Accepted Spread]
```

^-Only uses accepted heights that were not estimated.


## Historical Methods

### GOA/AI Spread (used through 2023)

``` mermaid
graph TD
    SA["Spread Data<br>(Both Vessels)"]-->SB["Gate Filter<br>10&lt;Spread&lt;22"]
    SB["Gate Filter<br>10&lt;Spread&lt;22"]-->SC["&gt;0 pings?"]
    SC["&gt;0 pings?"]-->|Yes| SS[Good Performance?]
    SC["&gt;0 pings?"]-->|No| SG[Good Performance?]
    SS[Good Performance?]-->|Yes| SU[Predictors]
    SC["&gt;0 pings?"]-->|Yes| SF[Accepted Spread]
    SG[Good Performance?]-->|Yes| SJ[Height and Net Number Available?]
    SG[Good Performance?]-->|No| SF[Accepted Spread]
    SD[Vessel<br>Net Number<br>Speed<br>Scope Ratio<br>Bottom Depth<br>Catch Weight<br>Good Obs. Height]-->SU[Predictors]
    subgraph Generalized Additive Models
      SU[Predictors]-.->SO[GAM: Spread]
      SU[Predictors]-.->SP[GAM: Spread<br>No NN]
      SU[Predictors]-.->SQ[GAM: Spread<br>No Ht]
      SU[Predictors]-.->SR[GAM: Spread<br>No Ht or NN]
      SJ[Height and Net Number Available?]-->|Ht: Yes<br>NN: Yes| SO[GAM: Spread]
      SJ[Height and Net Number Available?]-->|Ht: Yes<br>NN: No| SP[GAM: Spread<br>No NN]
      SJ[Height and Net Number Available?]-->|Ht: No<br>NN: Yes| SQ[GAM: Spread<br>No Ht]
      SJ[Height and Net Number Available?]-->|Ht: No<br>NN: No| SR[GAM: Spread<br>No Ht or NN]
    end
    SO[GAM: Spread]-->SF[Accepted Spread]
    SP[GAM: Spread<br>No NN]-->SF[Accepted Spread]
    SQ[GAM: Spread<br>No Ht]-->SF[Accepted Spread]
    SR[GAM: Spread<br>No Ht and NN]-->SF[Accepted Spread]
```

### EBS Spread (used through 2023)
``` mermaid
graph TD
    SA["Spread Data<br>(One Vessel)"]-->SB["Gate Filter<br>10&lt;Spread&lt;22"]
    SB["Gate Filter<br>10&lt;Spread&lt;22"]-->SC["&gt;50 pings?"]
    SC["&gt;50 pings?"]-->|No| SF["&gt;0 pings?"]
    SC["&gt;50 pings?"]-->|Yes| SN[Sequential Outlier<br>Rejection]
    SD[Inverse Scope<br><b>Accepted Height*</b>]-->SP[Predictors]
    SN[Sequential Outlier<br>Rejection]-->SO[Inspect Plot]
    SO[Inspect Plot]-->SP[Predictors]
    SF["&gt;0 pings?"]-->|No| SL[Spread GLM]
    SF["&gt;0 pings?"]-->|Yes| SI[Inspect plot:<br>Spread OK?]
    SP[Predictors]-.->SL[Spread GLM]
    SI[Inspect plot:<br>Spread OK?]-->|No| SL[Spread GLM]
    SI[Inspect plot:<br>Spread OK?]-->|Yes| SM[Accepted Spread]
    SL[Spread GLM]-->SM[Accepted Spread]
    SO[Inspect Plot]-->SM[Accepted Spread]
```

*-Includes accepted heights that were filled based on the average from other hauls.
