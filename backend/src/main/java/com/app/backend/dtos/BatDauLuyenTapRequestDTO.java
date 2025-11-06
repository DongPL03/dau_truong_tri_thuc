package com.app.backend.dtos;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.NotNull;
import lombok.*;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class BatDauLuyenTapRequestDTO {

    @NotNull
    @JsonProperty("bo_cau_hoi_id")
    private Long boCauHoiId;

    @JsonProperty("so_luong")
    private int soLuong = 10; // số câu ngẫu nhiên mặc định
}
