package com.app.backend.models;

import com.app.backend.models.constant.LuatTinhDiem;
import com.app.backend.models.constant.TrangThaiTranDau;
import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

@Entity
@Table(name = "tran_dau")
@Data
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class TranDau {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "bo_cau_hoi_id", nullable = false)
    private BoCauHoi boCauHoi;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "chu_phong_id", nullable = false)
    private NguoiDung chuPhong;

    @Column(name = "ma_phong", length = 10, nullable = false, unique = true)
    private String maPhong;

    @Column(name = "ten_phong")
    private String tenPhong;

    @Column(name = "trang_thai", columnDefinition = "ENUM('PENDING','ONGOING','FINISHED') DEFAULT 'PENDING'")
    private String trangThai = TrangThaiTranDau.PENDING;

    @Column(name = "cong_khai", columnDefinition = "BOOLEAN DEFAULT TRUE")
    private Boolean congKhai = true;

    @Column(name = "ma_pin", length = 10)
    private String maPin;

    @Column(name = "gioi_han_nguoi_choi", columnDefinition = "INT DEFAULT 5")
    private Integer gioiHanNguoiChoi = 5;

    @Column(name = "gioi_han_thoi_gian_cau_giay", columnDefinition = "INT DEFAULT 15")
    private Integer gioiHanThoiGianCauGiay = 15;

    @Column(name = "luat_tinh_diem", columnDefinition = "ENUM('BASIC','SPEED_BONUS') DEFAULT 'SPEED_BONUS'")
    private String luatTinhDiem = LuatTinhDiem.SPEED_BONUS;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "winner_id")
    private NguoiDung winner;

    @Column(name = "tao_luc", updatable = false)
    private LocalDateTime taoLuc;

    @Column(name = "bat_dau_luc")
    private LocalDateTime batDauLuc;

    @Column(name = "ket_thuc_luc")
    private LocalDateTime ketThucLuc;

    @PrePersist
    protected void onCreate() {
        taoLuc = LocalDateTime.now();
    }
}