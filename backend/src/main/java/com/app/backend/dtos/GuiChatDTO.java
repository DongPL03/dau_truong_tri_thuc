package com.app.backend.dtos;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

@Data
public class GuiChatDTO {

    @NotNull(message = "tran_dau_id là bắt buộc")
    @JsonProperty("tran_dau_id")
    private Long tranDauId;

    @NotBlank(message = "noi_dung không được để trống")
    @JsonProperty("noi_dung")
    private String noiDung;
}
