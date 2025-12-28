package com.app.backend.repositories;

import com.app.backend.models.NguoiChoiTranDau;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.Instant;
import java.util.List;
import java.util.Optional;

public interface INguoiChoiTranDauRepository extends JpaRepository<NguoiChoiTranDau, Long> {
    Page<NguoiChoiTranDau> findByTranDau_Id(Long tranDauId, Pageable pageable);

    Optional<NguoiChoiTranDau> findByTranDau_IdAndNguoiDung_Id(Long tranDauId, Long nguoiDungId);

    long countByTranDau_Id(Long tranDauId);

    Optional<NguoiChoiTranDau> findFirstByTranDau_IdOrderByIdAsc(Long tranDauId);

    List<NguoiChoiTranDau> findAllByTranDau_Id(Long tranDauId);

    boolean existsByTranDauIdAndNguoiDungId(Long tranDauId, Long nguoiDungId);

    /**
     * Lấy danh sách trận đấu của user sau một thời điểm nhất định (cho friend suggestions)
     */
    List<NguoiChoiTranDau> findByNguoiDung_IdAndThamGiaLucAfter(Long nguoiDungId, Instant after);
}
