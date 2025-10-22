package com.app.backend.models;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

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

    @Column(name = "xep_hang")
    private Integer xepHang;

    @Column(name = "mua_giai_id")
    private Long muaGiaiId; // Giả định Mùa giải (mua_giai) chưa được định nghĩa trong schema, nên dùng Long.

    @Column(name = "cap_nhat_luc")
    private LocalDateTime capNhatLuc;

    @PreUpdate
    protected void onUpdate() {
        capNhatLuc = LocalDateTime.now();
    }
}