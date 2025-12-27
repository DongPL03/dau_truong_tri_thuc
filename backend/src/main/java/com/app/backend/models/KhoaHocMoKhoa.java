package com.app.backend.models;

import com.fasterxml.jackson.annotation.JsonFormat;
import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.Instant;

@Entity
@Table(name = "khoa_hoc_mo_khoa", 
       uniqueConstraints = {
           @UniqueConstraint(columnNames = {"nguoi_dung_id", "khoa_hoc_id"})
       })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class KhoaHocMoKhoa {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "nguoi_dung_id", nullable = false)
    private NguoiDung nguoiDung;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "khoa_hoc_id", nullable = false)
    private KhoaHoc khoaHoc;

    @Column(name = "mo_khoa_luc")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant moKhoaLuc;
}

