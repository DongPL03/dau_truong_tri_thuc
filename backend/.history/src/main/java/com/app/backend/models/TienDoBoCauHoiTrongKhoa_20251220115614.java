package com.app.backend.models;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonFormat;
import jakarta.persistence.*;
import lombok.*;

import java.time.Instant;

@Entity
@Table(name = "tien_do_bo_cau_hoi_trong_khoa")
@Data
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class TienDoBoCauHoiTrongKhoa extends BaseEntity {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "tien_do_khoa_hoc_id", nullable = false)
    private TienDoKhoaHoc tienDoKhoaHoc;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "bo_cau_hoi_id", nullable = false)
    private BoCauHoi boCauHoi;

    @Column(name = "trang_thai", length = 50)
    private String trangThai; // CHUA_MO_KHOA, DA_MO_KHOA, DANG_HOC, HOAN_THANH

    @Column(name = "diem_cao_nhat")
    private Integer diemCaoNhat = 0;

    @Column(name = "so_lan_luyen_tap")
    private Integer soLanLuyenTap = 0;

    @Column(name = "lan_cuoi_luyen_tap")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'", timezone = "UTC")
    private Instant lanCuoiLuyenTap;
}

