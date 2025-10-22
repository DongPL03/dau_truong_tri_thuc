package com.app.backend.dtos;

import jakarta.validation.constraints.NotBlank;

public record UpdateRoleRequest(
        Long role // "ADMIN" | "USER"
) {}
