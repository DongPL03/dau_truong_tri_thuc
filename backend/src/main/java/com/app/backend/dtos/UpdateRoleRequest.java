package com.app.backend.dtos;

import jakarta.validation.constraints.NotBlank;
import lombok.*;

public record UpdateRoleRequest(
        Long role // "ADMIN" | "USER"
) {}
