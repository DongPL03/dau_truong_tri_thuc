package com.app.backend.dtos;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.*;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
public class CauHoiDTO {
    @NotNull
    @JsonProperty("bo_cau_hoi_id")
    private Long boCauHoiId;

    @JsonProperty("do_kho")
    private String doKho; // DE | TRUNG_BINH | KHO

    @NotBlank
    @JsonProperty("noi_dung")
    private String noiDung;

    @JsonProperty("loai_noi_dung")
    private String loaiNoiDung; // VAN_BAN | HINH_ANH | AM_THANH | VIDEO

    @JsonProperty("duong_dan_tep")
    private String duongDanTep;

    @NotBlank
    @JsonProperty("lua_chon_a")
    private String luaChonA;

    @NotBlank
    @JsonProperty("lua_chon_b")
    private String luaChonB;

    @NotBlank
    @JsonProperty("lua_chon_c")
    private String luaChonC;

    @NotBlank
    @JsonProperty("lua_chon_d")
    private String luaChonD;

    @JsonProperty("dap_an_dung")
    private Character dapAnDung; // A|B|C|D

    @JsonProperty("giai_thich")
    private String giaiThich;
}