package com.app.backend.dtos;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.NotNull;

/**
 * DTO để block một user
 */
public record BlockUserDTO(
        @NotNull(message = "target_user_id không được để trống")
        @JsonProperty("target_user_id")
        Long targetUserId,

        @JsonProperty("ly_do")
        String lyDo
) {}
