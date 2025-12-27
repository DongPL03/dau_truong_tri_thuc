package com.app.backend.models;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonFormat;
import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(name = "phan_tich_hoc_tap",
        uniqueConstraints = {
                @UniqueConstraint(columnNames = {"nguoi_dung_id", "khoa_hoc_id"})
        })
@Data
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class PhanTichHocTap {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "nguoi_dung_id", nullable = false)
    private NguoiDung nguoiDung;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "khoa_hoc_id", nullable = false)
    private KhoaHoc khoaHoc;

    @Column(name = "diem_manh", columnDefinition = "JSON")
    private String diemManh; // JSON array

    @Column(name = "diem_yeu", columnDefinition = "JSON")
    private String diemYeu; // JSON array

    @Column(name = "giai_phap", columnDefinition = "TEXT")
    private String giaiPhap;

    @Column(name = "chu_de_yeu", columnDefinition = "JSON")
    private String chuDeYeu; // JSON array

    @Column(name = "chu_de_manh", columnDefinition = "JSON")
    private String chuDeManh; // JSON array

    @Column(name = "cap_nhat_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant capNhatLuc;

    @PrePersist
    @PreUpdate
    protected void onUpdate() {
        capNhatLuc = Instant.now();
    }
}

