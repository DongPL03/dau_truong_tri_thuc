package com.app.backend.models;

import com.fasterxml.jackson.annotation.JsonFormat;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.Instant;

@Entity
@Table(name = "thanh_tich_bo_cau_hoi",
        uniqueConstraints = {
                @UniqueConstraint(columnNames = {"nguoi_dung_id", "bo_cau_hoi_id"})
        }
)
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class ThanhTichBoCauHoi {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "nguoi_dung_id", nullable = false)
    private NguoiDung nguoiDung;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "bo_cau_hoi_id", nullable = false)
    private BoCauHoi boCauHoi;

    @Column(name = "diem_cao_nhat", nullable = false)
    private Integer diemCaoNhat;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "tran_dau_id")
    private TranDau tranDau;

    @Column(name = "cap_nhat_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant capNhatLuc;

    @PrePersist
    @PreUpdate
    protected void onUpdate() {
        capNhatLuc = Instant.now();
    }
}

