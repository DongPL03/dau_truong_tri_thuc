package com.app.backend.models;

import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(name = "tags")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Tag {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "ten", nullable = false, length = 100)
    private String ten;

    @Column(name = "slug", nullable = false, unique = true, length = 100)
    private String slug;

    @Column(name = "mo_ta", length = 500)
    private String moTa;

    @Column(name = "mau_sac", length = 7)
    private String mauSac; // Hex color, e.g. #FF5733

    @Column(name = "icon", length = 50)
    private String icon; // Icon name or emoji

    @Column(name = "so_bai_viet")
    @Builder.Default
    private Integer soBaiViet = 0;

    @Column(name = "thu_tu")
    @Builder.Default
    private Integer thuTu = 0; // Thứ tự hiển thị

    @Column(name = "hien_thi")
    @Builder.Default
    private Boolean hienThi = true;

    @Column(name = "tao_luc")
    private Instant taoLuc;

    @Column(name = "cap_nhat_luc")
    private Instant capNhatLuc;

    @PrePersist
    protected void onCreate() {
        taoLuc = Instant.now();
        capNhatLuc = Instant.now();
    }

    @PreUpdate
    protected void onUpdate() {
        capNhatLuc = Instant.now();
    }
}
