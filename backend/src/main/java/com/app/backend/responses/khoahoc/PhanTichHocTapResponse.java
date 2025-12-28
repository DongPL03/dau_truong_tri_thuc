package com.app.backend.responses.khoahoc;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.Instant;
import java.util.List;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PhanTichHocTapResponse {
    @JsonProperty("id")
    private Long id;

    @JsonProperty("khoa_hoc_id")
    private Long khoaHocId;

    @JsonProperty("khoa_hoc_ten")
    private String khoaHocTen;

    @JsonProperty("diem_manh")
    private List<String> diemManh; // Danh sách điểm mạnh

    @JsonProperty("diem_yeu")
    private List<String> diemYeu; // Danh sách điểm yếu

    @JsonProperty("chu_de_manh")
    private List<ChuDeInfo> chuDeManh; // Danh sách chủ đề mạnh

    @JsonProperty("chu_de_yeu")
    private List<ChuDeInfo> chuDeYeu; // Danh sách chủ đề yếu

    @JsonProperty("giai_phap")
    private String giaiPhap; // Giải pháp cải thiện

    @JsonProperty("cap_nhat_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant capNhatLuc;

    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ChuDeInfo {
        @JsonProperty("id")
        private Long id;

        @JsonProperty("ten")
        private String ten;

        @JsonProperty("ti_le_dung")
        private Double tiLeDung; // Tỉ lệ đúng (%)
    }
}

