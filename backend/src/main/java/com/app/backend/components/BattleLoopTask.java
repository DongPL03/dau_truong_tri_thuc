package com.app.backend.components;

import com.app.backend.models.BattleState;
import com.app.backend.models.CauHoi;
import com.app.backend.models.TranDau;
import com.app.backend.models.constant.TrangThaiTranDau;
import com.app.backend.repositories.ITranDauRepository;
import com.app.backend.services.trandau.ITranDauService;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.List;

@Component
@RequiredArgsConstructor
public class BattleLoopTask {

    private final BattleStateManager battleStateManager;
    private final ITranDauRepository tranDauRepository;
    @Lazy
    @Autowired
    private ITranDauService tranDauService;  // ✅ TRÌ HOÃN KHỞI TẠO - cắt vòng lặp


    private final BattleWsPublisher wsPublisher;

    //    @Async
//    public void runAutoLoop(Long tranDauId, int secondsPerQuestion) {
//        BattleState state = battleStateManager.get(tranDauId);
//        if (state == null) return;
//        if (state.isAutoLoopRunning()) return;
//
//        state.setAutoLoopRunning(true);
//        if (state.getSecondsPerQuestion() <= 0)
//            state.setSecondsPerQuestion(secondsPerQuestion);
//        if (state.getStartTime() == null)
//            state.setStartTime(LocalDateTime.now());
//        battleStateManager.save(state);
//
//        TranDau td = tranDauRepository.findById(tranDauId).orElse(null);
//        if (td == null) {
//            state.setAutoLoopRunning(false);
//            battleStateManager.save(state);
//            return;
//        }
//
//        try {
//            wsPublisher.publishBattleStarted(
//                    tranDauId,
//                    td.getTenPhong() != null ? td.getTenPhong() : ("Phòng #" + tranDauId),
//                    state.getStartTime(),
//                    state.getDanhSachCauHoi().size(),
//                    state.getSecondsPerQuestion()
//            );
//
//            List<CauHoi> cauHoiList = state.getDanhSachCauHoi();
//            for (int i = 0; i < cauHoiList.size(); i++) {
//                BattleState latest = battleStateManager.get(tranDauId);
//                if (latest == null || !latest.isAutoLoopRunning()) break;
//
//                state.setCurrentQuestionIndex(i);
//                state.setCurrentQuestionStart(LocalDateTime.now());
//                battleStateManager.save(state);
//
//                CauHoi q = cauHoiList.get(i);
//                wsPublisher.publishNewQuestion(tranDauId, i, q, state.getSecondsPerQuestion());
//
//                try {
//                    Thread.sleep((long) state.getSecondsPerQuestion() * 1000L);
//                } catch (InterruptedException ie) {
//                    Thread.currentThread().interrupt();
//                    break;
//                }
//            }
//
//            if (state.markFinishedOnce()) {
//                td.setTrangThai(TrangThaiTranDau.FINISHED);
//                td.setKetThucLuc(state.getEndTime());
//                tranDauRepository.save(td);
//
//                Long hostId = (td.getChuPhong() != null) ? td.getChuPhong().getId() : null;
//                tranDauService.finishBattle(tranDauId, hostId, true);
//            }
//
//        } catch (Exception e) {
//            System.err.println("❌ Lỗi trong BattleLoopTask: " + e.getMessage());
//            e.printStackTrace();
//        } finally {
//            state.setAutoLoopRunning(false);
//            battleStateManager.save(state);
//            if (state.isFinished()) {
//                battleStateManager.remove(tranDauId);
//            }
//        }
//    }
    @Async
    public void runAutoLoop(Long tranDauId, int secondsPerQuestion) {
        BattleState state = battleStateManager.get(tranDauId);
        if (state == null) return;
        if (state.isAutoLoopRunning()) return;

        state.setAutoLoopRunning(true);
        if (state.getSecondsPerQuestion() <= 0) {
            state.setSecondsPerQuestion(secondsPerQuestion);
        }
        if (state.getStartTime() == null) {
            state.setStartTime(LocalDateTime.now());
        }
        battleStateManager.save(state);

        TranDau td = tranDauRepository.findById(tranDauId).orElse(null);
        if (td == null) {
            state.setAutoLoopRunning(false);
            battleStateManager.save(state);
            return;
        }

        try {
            int preCountdownSeconds = 10;
            // 🔔 Thông báo trận đấu bắt đầu
            wsPublisher.publishBattleStarted(
                    tranDauId,
                    td.getTenPhong() != null ? td.getTenPhong() : ("Phòng #" + tranDauId),
                    state.getStartTime(),
                    state.getDanhSachCauHoi().size(),
                    state.getSecondsPerQuestion(),
                    preCountdownSeconds
            );

            try {
                Thread.sleep(preCountdownSeconds * 1000L);
            } catch (InterruptedException ie) {
                Thread.currentThread().interrupt();
                return;
            }


            List<CauHoi> cauHoiList = state.getDanhSachCauHoi();
            for (int i = 0; i < cauHoiList.size(); i++) {
                // luôn lấy state mới nhất
                BattleState latest = battleStateManager.get(tranDauId);
                if (latest == null || !latest.isAutoLoopRunning()) {
                    break; // có ai đó stop loop
                }

                latest.setCurrentQuestionIndex(i);
                latest.setCurrentQuestionStart(LocalDateTime.now());
                battleStateManager.save(latest);

                CauHoi q = cauHoiList.get(i);
                wsPublisher.publishNewQuestion(tranDauId, i, q, latest.getSecondsPerQuestion());

                try {
                    Thread.sleep(latest.getSecondsPerQuestion() * 1000L);
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                    break;
                }
            }

            // ❗❗ HẾT CÂU HỎI → CHỈ GỌI SERVICE, KHÔNG TỰ SET FINISHED
            Long hostId = (td.getChuPhong() != null) ? td.getChuPhong().getId() : null;
            System.out.println(">>> [LOOP] Hết câu hỏi, gọi finishBattle(auto), tranDauId=" + tranDauId);
            tranDauService.finishBattle(tranDauId, hostId, true);

        } catch (Exception e) {
            System.err.println("❌ Lỗi trong BattleLoopTask: " + e.getMessage());
            e.printStackTrace();
        } finally {
            state.setAutoLoopRunning(false);
            battleStateManager.save(state);
            // ❌ KHÔNG remove state ở đây, đã có finishBattle xử lý
            // battleStateManager.remove(tranDauId);
        }
    }

}
