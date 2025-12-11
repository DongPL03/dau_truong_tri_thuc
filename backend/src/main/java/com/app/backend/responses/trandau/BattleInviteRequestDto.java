package com.app.backend.responses.trandau;

import com.fasterxml.jackson.annotation.JsonProperty;

public record BattleInviteRequestDto(
        @JsonProperty("target_user_id")
        Long targetUserId
) {
}
