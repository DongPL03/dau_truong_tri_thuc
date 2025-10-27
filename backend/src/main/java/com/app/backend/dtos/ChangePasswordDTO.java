package com.app.backend.dtos;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import lombok.*;

public record ChangePasswordDTO(
        @NotBlank String oldPassword,
        @NotBlank @Size(min = 6, max = 72) String newPassword
) {
}