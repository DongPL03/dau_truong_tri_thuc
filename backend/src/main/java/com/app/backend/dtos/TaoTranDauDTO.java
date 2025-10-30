package com.app.backend.dtos;

import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import lombok.*;

@Data
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class TaoTranDauDTO {

    @NotNull(message = "boCauHoiId là bắt buộc")
    @JsonProperty("bo_cau_hoi_id")
    private Long boCauHoiId;

    @JsonProperty("ten_phong")
    private String tenPhong;          // Tên hiển thị (vd: "Phòng luyện tập 1")

    @JsonProperty("cong_khai")
    private Boolean congKhai = true;  // Public/Private

    @JsonProperty("ma_pin")
    private String maPin;             // Dùng khi phòng private, có thể null

    @Min(2)
    @JsonProperty("gioi_han_nguoi_choi")
    private Integer gioiHanNguoiChoi = 2;

    @Min(5)
    @JsonProperty("gioi_han_thoi_gian_cau_giay")
    private Integer gioiHanThoiGianCauGiay = 15; // tính bằng giây (thay vì ms)

    @JsonProperty("luat_tinh_diem")
    private String luatTinhDiem = "SPEED_BONUS"; // hoặc "BASIC"
}