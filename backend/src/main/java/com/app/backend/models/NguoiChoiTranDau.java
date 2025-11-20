package com.app.backend.models;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonFormat;
import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(name = "nguoi_choi_tran_dau", uniqueConstraints = {
        @UniqueConstraint(columnNames = {"tran_dau_id", "nguoi_dung_id"})
})
@Data
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class NguoiChoiTranDau {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "tran_dau_id", nullable = false)
    private TranDau tranDau;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "nguoi_dung_id", nullable = false)
    private NguoiDung nguoiDung;

    @Column(name = "diem", columnDefinition = "INT DEFAULT 0")
    private Integer diem = 0;

    @Column(name = "so_cau_dung", columnDefinition = "INT DEFAULT 0")
    private Integer soCauDung = 0;

    @Column(name = "xep_hang")
    private Integer xepHang;

    @Column(name = "tham_gia_luc", updatable = false)
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant thamGiaLuc;

    @PrePersist
    protected void onCreate() {
        thamGiaLuc = Instant.now();
    }
}