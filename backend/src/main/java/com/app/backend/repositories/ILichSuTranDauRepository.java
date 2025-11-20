package com.app.backend.repositories;

import com.app.backend.models.LichSuTranDau;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface ILichSuTranDauRepository extends JpaRepository<LichSuTranDau, Long> {
    void deleteByTranDau_Id(Long tranDauId);

    List<LichSuTranDau> findAllByTranDau_Id(Long tranDauId);

    Page<LichSuTranDau> findByNguoiDung_IdOrderByHoanThanhLucDesc(Long nguoiDungId, Pageable pageable);

    // dùng cho detail
    Optional<LichSuTranDau> findByTranDau_IdAndNguoiDung_Id(Long tranDauId, Long nguoiDungId);

    List<LichSuTranDau> findByTranDau_IdOrderByXepHangAsc(Long tranDauId);
}
