package com.app.backend.repositories;

import com.app.backend.models.CauHoi;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ICauHoiRepository extends JpaRepository<CauHoi, Long> {
}
