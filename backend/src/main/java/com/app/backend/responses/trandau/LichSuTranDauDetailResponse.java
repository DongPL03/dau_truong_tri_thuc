package com.app.backend.responses.trandau;

import com.app.backend.models.LichSuTranDau;
import com.app.backend.models.TranDau;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.*;

import java.time.Instant;
import java.time.LocalDateTime;
import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class LichSuTranDauDetailResponse {

    // thông tin trận
    @JsonProperty("tran_dau_id")
    private Long tranDauId;

    @JsonProperty("ten_phong")
    private String tenPhong;

    @JsonProperty("ma_phong")
    private String maPhong;

    @JsonProperty("bo_cau_hoi_tieu_de")
    private String boCauHoiTieuDe;

    @JsonProperty("luat_tinh_diem")
    private String luatTinhDiem;

    @JsonProperty("bat_dau_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant batDauLuc;

    @JsonProperty("ket_thuc_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant ketThucLuc;

    // thống kê của chính user
    @JsonProperty("my_tong_diem")
    private Integer myTongDiem;

    @JsonProperty("my_so_cau_dung")
    private Integer mySoCauDung;

    @JsonProperty("my_xep_hang")
    private Integer myXepHang;

    @JsonProperty("my_tong_thoi_gian_ms")
    private Long myTongThoiGianMs;

    // leaderboard chung cuộc
    @JsonProperty("leaderboard")
    private List<FinishedPlayer> leaderboard;

    // danh sách câu hỏi mà user đã trả lời trong trận
    @JsonProperty("questions")
    private List<LichSuTranDauQuestionResponse> questions;


    public static LichSuTranDauDetailResponse baseFrom(TranDau td, LichSuTranDau myHistory) {
        return LichSuTranDauDetailResponse.builder()
                .tranDauId(td.getId())
                .tenPhong(td.getTenPhong())
                .maPhong(td.getMaPhong())
                .boCauHoiTieuDe(
                        td.getBoCauHoi() != null ? td.getBoCauHoi().getTieuDe() : null
                )
                .luatTinhDiem(td.getLuatTinhDiem())
                .batDauLuc(td.getBatDauLuc())
                .ketThucLuc(td.getKetThucLuc())
                .myTongDiem(myHistory.getTongDiem())
                .mySoCauDung(myHistory.getSoCauDung())
                .myXepHang(myHistory.getXepHang())
                .myTongThoiGianMs(Long.valueOf(myHistory.getTongThoiGianMs()))
                .build();
    }
}

