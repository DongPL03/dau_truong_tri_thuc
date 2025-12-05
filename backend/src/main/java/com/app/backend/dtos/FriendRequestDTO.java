package com.app.backend.dtos;

import jakarta.validation.constraints.NotNull;

public record FriendRequestDTO(
        @NotNull(message = "target_user_id is required")
        Long target_user_id
) {
}