package com.app.backend.responses.trandau;

import com.app.backend.models.constant.RankTier;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class MatchRewardResponse {

    @JsonProperty("xp_gained")
    private Long xpGained;

    @JsonProperty("gold_gained")
    private Long goldGained;

    @JsonProperty("level_before")
    private Integer levelBefore;

    @JsonProperty("level_after")
    private Integer levelAfter;

    @JsonProperty("rank_tier_before")
    private RankTier rankTierBefore;

    @JsonProperty("rank_tier_after")
    private RankTier rankTierAfter;
}
