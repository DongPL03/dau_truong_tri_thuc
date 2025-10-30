package com.app.backend.dtos;

public record UpdateRoleRequestDTO(
        Long role // "ADMIN" | "USER"
) {}
