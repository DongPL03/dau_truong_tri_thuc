package com.app.backend.responses.thongke;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class TopicStatResponse {

    @JsonProperty("chu_de_id")
    private Long chuDeId;

    @JsonProperty("chu_de_ten")
    private String chuDeTen;

    @JsonProperty("tong_cau")
    private Long tongCau;

    @JsonProperty("so_cau_dung")
    private Long soCauDung;

    @JsonProperty("ti_le_dung")
    private Double tiLeDung;

    @JsonProperty("danh_gia")
    private String danhGia;

    // --- Constructor thông minh ---
    // Hibernate sẽ gọi Constructor này. Logic tính toán nằm gọn ở đây.
    public TopicStatResponse(Long chuDeId, String chuDeTen, Long tongCau, Long soCauDung) {
        this.chuDeId = chuDeId;
        this.chuDeTen = chuDeTen;
        this.tongCau = tongCau;
        this.soCauDung = soCauDung != null ? soCauDung : 0L; // Xử lý null từ hàm SUM

        // 1. Tính tỉ lệ đúng
        if (this.tongCau > 0) {
            this.tiLeDung = (double) this.soCauDung / this.tongCau * 100.0;
        } else {
            this.tiLeDung = 0.0;
        }

        // 2. Tính đánh giá (Logic nghiệp vụ đưa vào đây)
        if (this.tongCau < 5) {
            this.danhGia = "TRUNG_BINH"; // Ít dữ liệu quá thì chưa đánh giá được
        } else if (this.tiLeDung >= 70.0) {
            this.danhGia = "MANH";
        } else if (this.tiLeDung >= 40.0) {
            this.danhGia = "TRUNG_BINH";
        } else {
            this.danhGia = "YEU";
        }
    }
}