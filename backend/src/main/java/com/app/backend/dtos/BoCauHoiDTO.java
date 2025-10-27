package com.app.backend.dtos;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Data
public class BoCauHoiDTO {
    @NotBlank
    private String tieuDe;

    private String moTa;

    @NotNull
    private Long chuDeId; // FK -> ChuDe

    @NotBlank
    private String cheDoHienThi; // PUBLIC | PRIVATE
}
