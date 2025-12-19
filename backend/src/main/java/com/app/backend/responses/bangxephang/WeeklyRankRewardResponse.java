package com.app.backend.responses.bangxephang;

import com.app.backend.models.constant.RankTier;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class WeeklyRankRewardResponse {

    @JsonProperty("gold_reward")
    private Long goldReward;

    @JsonProperty("rank_tier")
    private RankTier rankTier;

    @JsonProperty("global_rank")
    private Integer globalRank;

    @JsonProperty("week_id")
    private String weekId;      // ví dụ "2025-51"

    @JsonProperty("claimed_before")
    private boolean claimedBefore;

    @JsonProperty("gold_before")
    private Long goldBefore;

    @JsonProperty("gold_after")
    private Long goldAfter;
}
