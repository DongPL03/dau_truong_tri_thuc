package com.app.backend.models;

import com.fasterxml.jackson.annotation.JsonBackReference;
import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "khoa_hoc_bo_cau_hoi",
        uniqueConstraints = {
                @UniqueConstraint(columnNames = {"khoa_hoc_id", "thu_tu"})
        })
@Data
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class KhoaHocBoCauHoi {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "khoa_hoc_id", nullable = false)
    private KhoaHoc khoaHoc;

    @ManyToOne(fetch = FetchType.LAZY)
    @JsonBackReference
    @JoinColumn(name = "bo_cau_hoi_id", nullable = false)
    private BoCauHoi boCauHoi;

    @Column(name = "thu_tu", nullable = false)
    private Integer thuTu;

    @Column(name = "is_bat_buoc", columnDefinition = "TINYINT(1) DEFAULT 1")
    private Boolean isBatBuoc = true;

    @Column(name = "diem_toi_thieu")
    private Integer diemToiThieu = 0;
}

