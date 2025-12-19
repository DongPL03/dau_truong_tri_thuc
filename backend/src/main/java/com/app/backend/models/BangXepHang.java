package com.app.backend.models;

import com.app.backend.models.constant.RankTier;
import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(name = "bang_xep_hang")
@Data
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class BangXepHang {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @OneToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "nguoi_dung_id", nullable = false, unique = true)
    private NguoiDung nguoiDung;

    @Column(name = "tong_diem", columnDefinition = "INT DEFAULT 0")
    private Integer tongDiem = 0;

    @Column(name = "tong_tran", columnDefinition = "INT DEFAULT 0")
    private Integer tongTran = 0;

    @Column(name = "tuan_thuong_hang_cuoi_cung", length = 10)
    private String lastRankRewardWeek; // ví dụ "2025-51"

    @Column(name = "so_tran_thang")
    private Integer soTranThang = 0;

    @Column(name = "so_tran_thua")
    private Integer soTranThua = 0;

    @Column(name = "xep_hang")
    private Integer xepHang;

    @Column(name = "mua_giai_id")
    private Long muaGiaiId; // Giả định Mùa giải (mua_giai) chưa được định nghĩa trong schema, nên dùng Long.

    @Column(name = "tong_xp")
    @JsonProperty("tong_xp")
    private Long tongXp = 0L;

    @Column(name = "cap_do")
    @JsonProperty("level")
    private Integer level = 1;

    @Enumerated(EnumType.STRING) // Quan trọng: Báo cho Hibernate lưu tên Enum (ví dụ "GOLD") vào DB
    @Column(name = "thu_hang", columnDefinition = "ENUM('BRONZE','SILVER','GOLD','PLATINUM','DIAMOND','MASTER')")
    private RankTier rankTier = RankTier.BRONZE; // Dùng kiểu Enum, không dùng String

    @Column(name = "tien_vang")
    @JsonProperty("tien_vang")
    private Long tienVang = 0L;

    @Column(name = "cap_nhat_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant capNhatLuc;

    @PreUpdate
    protected void onUpdate() {
        capNhatLuc = Instant.now();
    }
}