package com.app.backend.dtos;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;

public record ChangePasswordDTO(
        @NotBlank
        @JsonProperty("old_password")
        String oldPassword,
        @NotBlank
        @Size(min = 6, max = 72)
        @JsonProperty("new_password")
        String newPassword
) {
}