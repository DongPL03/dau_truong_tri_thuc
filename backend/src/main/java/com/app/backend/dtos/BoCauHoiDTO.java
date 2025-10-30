package com.app.backend.dtos;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Data
public class BoCauHoiDTO {

    @NotBlank
    @JsonProperty("tieu_de")
    private String tieuDe;

    @JsonProperty("mo_ta")
    private String moTa;

    @NotNull
    @JsonProperty("chu_de_id")
    private Long chuDeId; // FK -> ChuDe

    @NotBlank
    @JsonProperty("che_do_hien_thi")
    private String cheDoHienThi; // PUBLIC | PRIVATE
}
